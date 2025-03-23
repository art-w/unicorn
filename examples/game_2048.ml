let () = Random.self_init ()

module type CELL = sig
  type t

  val view : t -> int
  val make : int -> t
  val ( + ) : t -> t -> t
end

module Cell_int = struct
  type t = int

  let view t = t
  let make t = t
  let ( + ) a b = a + b
end

module Rules (C : CELL) = struct
  type t = C.t option list list

  let empty =
    [ [ None; None; None; None ]
    ; [ None; None; None; None ]
    ; [ None; None; None; None ]
    ; [ None; None; None; None ]
    ]

  let shift_line line =
    let rec go acc = function
      | [] -> acc
      | None :: rest -> go (None :: acc) rest
      | x :: None :: rest -> go (None :: acc) (x :: rest)
      | Some x :: Some y :: rest when C.view x = C.view y ->
        Some C.(x + y) :: go (None :: acc) rest
      | x :: rest -> x :: go acc rest
    in
    go [] line

  let wrap f g x = f (g (f x))
  let inverse = List.map List.rev

  let rec transpose = function
    | [] :: _ -> []
    | xss -> List.map List.hd xss :: transpose (List.map List.tl xss)

  let shift_left = List.map shift_line
  let shift_right = wrap inverse shift_left
  let shift_down = wrap transpose shift_right
  let shift_up = wrap transpose shift_left

  let make_at xy v =
    List.mapi
    @@ fun y' -> List.mapi @@ fun x' c -> if xy = (x', y') then Some (C.make v) else c

  let add_random_cell game =
    let candidates =
      List.concat
      @@ List.mapi
           (fun y row ->
              List.concat
              @@ List.mapi
                   (fun x -> function
                      | None -> [ x, y ]
                      | _ -> [])
                   row)
           game
    in
    match List.length candidates with
    | 0 -> invalid_arg "game over!"
    | nb ->
      let xy = List.nth candidates (Random.int nb) in
      let v = 2 * (1 + Random.int 2) in
      make_at xy v game

  let equal =
    List.for_all2
    @@ List.for_all2
    @@ fun ox oy ->
    match ox, oy with
    | None, None -> true
    | Some x, Some y -> C.view x = C.view y
    | _ -> false

  let is_gameover b =
    List.for_all
      (fun f -> equal (f b) b)
      [ shift_left; shift_right; shift_down; shift_up ]

  let step act game =
    let new_game = act game in
    if equal game new_game then game else add_random_cell new_game

  let step_left = step shift_left
  let step_right = step shift_right
  let step_down = step shift_down
  let step_up = step shift_up
  let create () = add_random_cell (add_random_cell empty)
end

module Test = struct
  module Game = Rules (Cell_int)

  let () =
    let b =
      Game.shift_right
        [ [ None; None; None; None ]
        ; [ Some 2; None; Some 2; None ]
        ; [ None; None; None; None ]
        ; [ Some 4; Some 2; None; Some 4 ]
        ]
    in
    let expect =
      [ [ None; None; None; None ]
      ; [ None; None; None; Some 4 ]
      ; [ None; None; None; None ]
      ; [ None; Some 4; Some 2; Some 4 ]
      ]
    in
    assert (Game.equal b expect)
end

(********************************************************************************)

type color = int * int * int

let rgb r g b = r, g, b

module Anim = struct
  type 'a t =
    { duration : float
    ; at : float
    ; view : float -> 'a
    }

  let tick t dt = { t with at = t.at +. dt }
  let is_done t = t.at > t.duration
  let pure x = { duration = 0.0; at = 0.0; view = (fun _ -> x) }

  let ( <$> ) a b =
    { duration = max (a.duration -. a.at) (b.duration -. b.at)
    ; at = 0.0
    ; view = (fun t -> (a.view (t +. a.at)) (b.view (t +. b.at)))
    }

  let final t = t.view t.duration
  let view t = t.view t.at
  let clamp t = max 0.0 (min t 1.0)
  let const ?(duration = 1000.0) v = { duration; at = 0.0; view = (fun _ -> v) }

  let duration d a =
    if a.duration = 0.0
    then const ~duration:d (a.view 0.0)
    else
      { duration = d
      ; at = a.at *. d /. a.duration
      ; view = (fun t -> a.view (t *. a.duration /. d))
      }

  let seq a b =
    { duration = a.duration +. b.duration
    ; at = a.at
    ; view = (fun t -> if t < a.duration then a.view t else b.view (t -. a.duration))
    }

  let ( >>| ) a f = seq a (f (final a))

  let tween ?(duration = 1000.0) x0 x1 =
    { duration
    ; at = 0.0
    ; view =
        (fun t ->
          let dt = clamp (t /. duration) in
          let dt = dt *. dt in
          x0 +. ((x1 -. x0) *. dt))
    }

  let tween_to ?duration x1 x0 = tween ?duration x0 x1

  let tween_int ?duration x0 x1 =
    pure int_of_float <$> tween ?duration (float x0) (float x1)

  let tween_rgb (r0, g0, b0) (r1, g1, b1) =
    pure rgb <$> tween_int r0 r1 <$> tween_int g0 g1 <$> tween_int b0 b1
end

type 'a anim = 'a Anim.t

(********************************************************************************)

let whiteish = rgb 0xf9 0xf6 0xf2
let brownish = rgb 0x77 0x6e 0x65

let color_of_value = function
  | 2 -> rgb 0xee 0xe4 0xda, brownish
  | 4 -> rgb 0xed 0xe0 0xc8, brownish
  | 8 -> rgb 0xf2 0xb1 0x79, whiteish
  | 16 -> rgb 0xf5 0x95 0x63, whiteish
  | 32 -> rgb 0xf6 0x7c 0x5f, whiteish
  | 64 -> rgb 0xf6 0x5e 0x3b, whiteish
  | 128 -> rgb 0xed 0xcf 0x72, whiteish
  | 256 -> rgb 0xed 0xcc 0x61, whiteish
  | 512 -> rgb 0xed 0xc8 0x50, whiteish
  | 1024 -> rgb 0xed 0xc5 0x3f, whiteish
  | 2048 -> rgb 0xed 0xc2 0x2e, whiteish
  | _ -> brownish, brownish

type style =
  { value : int
  ; pos : (float * float) option
  ; opacity : float
  ; size : float
  ; removed : bool
  ; fg : color
  ; bg : color
  }

module Cell_anim = struct
  open Anim

  type t = style anim * style anim list

  let view (t, _) = (Anim.final t).value

  let make value =
    let bg, fg = color_of_value value in
    let s = { value; pos = None; removed = false; opacity = 0.0; size = 0.0; fg; bg } in
    let anim =
      let open Anim in
      pure (fun opacity size -> { s with opacity; size })
      <$> (const 0.0 >>| tween_to 1.0)
      <$> (const 0.0 >>| tween_to 1.0)
    in
    duration 1250.0 anim, []

  let disappear t =
    let c = Anim.view t in
    pure (fun s opacity size -> { s with opacity; size })
    <$> t
    <$> tween c.opacity 0.0
    <$> tween c.size 0.0
    >>| fun s -> pure { s with removed = true }

  let bump new_value t =
    let bg, fg = color_of_value new_value in
    let c = Anim.view t in
    pure (fun s value size fg bg -> { s with size; fg; bg; value })
    <$> t
    <$> tween_int c.value new_value
    <$> (tween c.size 1.3 >>| tween_to 1.0)
    <$> tween_rgb c.fg fg
    <$> tween_rgb c.bg bg

  let ( + ) (x, xs) (y, ys) =
    let a, b = Anim.final x, Anim.final y in
    let value = a.value + b.value in
    bump value x, List.rev_append xs (disappear y :: ys)

  let translate (x, y) t =
    let x', y' =
      match (Anim.view t).pos with
      | None -> x, y
      | Some (x', y') -> x', y'
    in
    pure (fun t x y -> { t with pos = Some (x, y) }) <$> t <$> tween x' x <$> tween y' y

  let goto xy t =
    match (Anim.final t).pos with
    | Some xy' when xy = xy' -> t
    | _ -> translate xy t

  let set_pos (x, y) (t, children) =
    let xy = float x, float y in
    goto xy t, List.map (goto xy) children
end

module Game = Rules (Cell_anim)

(********************************************************************************)

open Unicorn_jsoo

let square_size = 60.0

let css_translate (x, y) =
  Printf.sprintf "transform: translate(%fpx, %fpx)" (square_size *. x) (square_size *. y)

let css_rgb (r, g, b) = Printf.sprintf "rgb(%i,%i,%i)" r g b

let background_tile (x, y) =
  H.div (A.class_ (fun _ -> "tile") & A.style (fun _ -> css_translate (float x, float y)))

let square =
  let style s =
    let x, y =
      match s.pos with
      | None -> 0.0, 0.0
      | Some (x, y) -> x, y
    in
    Printf.sprintf
      "color: %s; background-color: %s; opacity: %f; %s scale(%f)"
      (css_rgb s.fg)
      (css_rgb s.bg)
      s.opacity
      (css_translate (x, y))
      s.size
  in
  H.div
    (A.class_ (fun _ -> "square")
     & A.style style
     & (text @@ fun s -> string_of_int s.value))

let animate =
  cond (fun anim -> not (Anim.is_done anim))
  @@ E.dt (fun dt anim -> Anim.tick anim (5.0 *. dt))

let square = animate & on { Optic.get = Anim.view; put = (fun _ a -> a) } square
let remove_done = List.filter (fun a -> not (Anim.view a).removed)
let remove_done = { Optic.ltor = remove_done; rtol = remove_done }
let cell = on Optic.Lens.snd (iso remove_done (list square)) & on Optic.Lens.fst square

let list_index fn =
  let rec go i = into Optic.Prism.cons (fn i <*> apply go (i + 1)) in
  go 0

let matrix_index w = list_index (fun y -> list_index (fun x -> w (x, y)))

let set_position xy =
  { Optic.ltor = (fun anim -> Cell_anim.set_pos xy anim); rtol = (fun anim -> anim) }

let board = matrix_index @@ fun xy -> into Optic.Prism.some @@ iso (set_position xy) cell
let background = matrix_index background_tile
let game = H.div (A.class_ (fun _ -> "board") & background & board)

let gameover_ui =
  H.h1 (str "Game Over!") & button (str "New Game" & E.click (fun _ -> Game.create ()))

let toolbar =
  let btn name act = button (str name & E.click act) in
  A.tabindex (fun _ -> 0)
  & E.keydown (function
    | 37 -> Game.step_left
    | 38 -> Game.step_up
    | 39 -> Game.step_right
    | 40 -> Game.step_down
    | _ -> fun g -> g)
  & H.div
      (btn "LEFT" Game.step_left
       & btn "RIGHT" Game.step_right
       & btn "UP" Game.step_up
       & btn "DOWN" Game.step_down)

let toolbar = ifte Game.is_gameover gameover_ui toolbar
let main = H.div (A.class_ (fun _ -> "game") & H.div (toolbar & game))
let () = run main (Game.create ())
