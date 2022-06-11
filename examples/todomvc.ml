open Optic
open Unicorn_jsoo

type todo =
  { id : int
  ; message : string
  ; completed : bool
  }
[@@deriving optic]

let add_todo message todos =
  match message with
  | "" -> todos
  | _ ->
    let id = List.fold_left (fun id t -> max id (1 + t.id)) 0 todos in
    { id; message; completed = false } :: todos

let todo =
  H.checkbox completed [ A.class_ (fun _ -> "toggle") ]
  & H.label (text (fun t -> t.message))

let removable w =
  H.div
    (A.class_ (fun _ -> "view")
    & into Prism.some w
    & H.button (A.class_ (fun _ -> "destroy") & E.click (fun _ -> None)))

let todo_edit =
  H.input_string
    Lens.(compose snd message)
    [ A.class_ (fun _ -> "edit")
    ; E.init_focus ()
    ; E.blur (fun (_, t) -> false, t)
    ; E.keydown (fun key (_, t) -> key <> 13, t)
    ]

let todo =
  stateful
    false
    (into
       Prism.(product id some)
       (A.class_ (fun (edit, t) ->
            let ok = if t.completed then "completed" else "not-completed" in
            let edit = if edit then "editing" else "" in
            ok ^ " " ^ edit)
       & on Lens.fst (E.doubleclick (fun _ -> true)))
    & on Lens.snd (removable todo)
    & case Prism.(product (is true) some) todo_edit)

let removable_cons =
  { Prism.extract =
      (function
      | x :: xs -> Some (Some x, xs)
      | [] -> None)
  ; make =
      (function
      | Some x, xs -> x :: xs
      | None, xs -> xs)
  }

let removable_list w =
  let rec lst = lazy (into removable_cons (on Lens.snd (of_lazy lst) & on Lens.fst w)) in
  Lazy.force lst

let todolist = H.ul (A.class_ (fun _ -> "todo-list") & removable_list (H.li todo))

let todo_input =
  stateful
    ""
    (H.input_string
       Lens.fst
       [ A.class_ (fun _ -> "new-todo")
         & A.placeholder (fun _ -> "What's needs to be done?")
         & A.autofocus (fun _ -> true)
         & E.keydown (fun key (x, xs) -> if key = 13 then "", add_todo x xs else x, xs)
       ])

let header = H.div (H.header (H.h1 (str "todos") & todo_input))

let all_completed =
  { Lens.get = (fun xs -> List.for_all (fun x -> x.completed) xs)
  ; put = (fun completed xs -> List.map (fun x -> { x with completed }) xs)
  }

let toggle_all =
  H.checkbox
    all_completed
    [ A.id (fun _ -> "toggle-all") & A.class_ (fun _ -> "toggle-all") ]
  & H.label (A.for_ (fun _ -> "toggle-all") & str "Mark all as complete")

let applied =
  { Lens.get = (fun (lens, x) -> Lens.get lens x)
  ; put = (fun y (lens, x) -> lens, Lens.put lens y x)
  }

let main =
  H.section (A.class_ (fun _ -> "main") & on Lens.snd toggle_all & on applied todolist)

let count_todos xs =
  match List.length (List.filter (fun t -> not t.completed) xs) with
  | 1 -> "1 item left"
  | n -> Printf.sprintf "%i items left" n

let all = Lens.id

module M = Map.Make (Int)

let replace pred xs ys =
  let m = List.fold_left (fun m x -> M.add x.id x m) M.empty xs in
  List.filter_map (fun y -> if pred y then M.find_opt y.id m else Some y) ys

let filter_by pred = { Lens.get = (fun xs -> List.filter pred xs); put = replace pred }
let active = filter_by (fun t -> not t.completed)
let completed = filter_by (fun t -> t.completed)

let filter name lens =
  H.li
    (H.a
       (cond (fun lens' -> lens == lens') (A.class_ (fun _ -> "selected"))
       & str name
       & E.click (fun _ -> lens)))

let filters =
  H.ul
    (A.class_ (fun _ -> "filters")
    & filter "All" all
    & filter "Active" active
    & filter "Completed" completed)

let clear_completed =
  cond (List.exists (fun t -> t.completed))
  @@ H.button
       (A.class_ (fun _ -> "clear-completed")
       & E.click (fun xs -> Lens.get active xs)
       & str "Clear Completed")

let footer =
  H.footer
    (A.class_ (fun _ -> "footer")
    & on Lens.snd (H.span (A.class_ (fun _ -> "todo-count") & text count_todos))
    & on Lens.fst filters
    & on Lens.snd clear_completed)

let app = H.div (header & stateful all (main & footer))
let () = run ~id:"app" app []
