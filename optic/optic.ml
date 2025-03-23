type ('a, 'b) iso =
  { ltor : 'a -> 'b
  ; rtol : 'b -> 'a
  }

type ('a, 'b) lens =
  { get : 'a -> 'b
  ; put : 'b -> 'a -> 'a
  }

type ('a, 'b) prism =
  { extract : 'a -> 'b option
  ; make : 'b -> 'a
  }

module Iso = struct
  type ('a, 'b) t = ('a, 'b) iso =
    { ltor : 'a -> 'b
    ; rtol : 'b -> 'a
    }

  let ltor t = t.ltor
  let rtol t = t.rtol

  let compose ab bc =
    { ltor = (fun a -> bc.ltor (ab.ltor a)); rtol = (fun c -> ab.rtol (bc.rtol c)) }

  let ( @@ ) = compose

  let product ab cd =
    { ltor = (fun (a, c) -> ab.ltor a, cd.ltor c)
    ; rtol = (fun (b, d) -> ab.rtol b, cd.rtol d)
    }

  let ( <*> ) = product
  let map iso f x = iso.rtol (f (iso.ltor x))

  let assoc =
    { ltor = (fun ((a, b), c) -> a, (b, c)); rtol = (fun (a, (b, c)) -> (a, b), c) }
end

module Lens = struct
  type ('a, 'b) t = ('a, 'b) lens =
    { get : 'a -> 'b
    ; put : 'b -> 'a -> 'a
    }

  let get t = t.get
  let put t = t.put
  let map { get; put } fn x = put (fn (get x)) x

  let compose ab bc =
    { get = (fun a -> bc.get (ab.get a)); put = (fun c a -> map ab (bc.put c) a) }

  let ( @@ ) = compose

  let product ab cd =
    { get = (fun (a, c) -> ab.get a, cd.get c)
    ; put = (fun (b, d) (a, c) -> ab.put b a, cd.put d c)
    }

  let ( <*> ) = product
  let id = { get = (fun x -> x); put = (fun x _ -> x) }
  let fst = { get = (fun (x, _) -> x); put = (fun x (_, y) -> x, y) }
  let snd = { get = (fun (_, y) -> y); put = (fun y (x, _) -> x, y) }
end

module Prism = struct
  type ('a, 'b) t = ('a, 'b) prism =
    { extract : 'a -> 'b option
    ; make : 'b -> 'a
    }

  let extract t = t.extract
  let make t = t.make

  let map t f x =
    match t.extract x with
    | None -> x
    | Some y -> t.make (f y)

  let id = { extract = (fun x -> Some x); make = (fun x -> x) }

  let compose ab bc =
    { extract = (fun a -> Option.bind (ab.extract a) bc.extract)
    ; make = (fun c -> ab.make (bc.make c))
    }

  let product ab cd =
    { extract =
        (fun (a, c) ->
          match ab.extract a, cd.extract c with
          | Some b, Some d -> Some (b, d)
          | _ -> None)
    ; make = (fun (b, d) -> ab.make b, cd.make d)
    }

  let satisfy predicate =
    { extract = (fun x -> if predicate x then Some x else None); make = (fun x -> x) }

  let is v = { extract = (fun x -> if x = v then Some x else None); make = (fun x -> x) }

  let none =
    { extract =
        (function
          | None -> Some ()
          | _ -> None)
    ; make = (fun () -> None)
    }

  let some = { extract = (fun opt -> opt); make = (fun x -> Some x) }

  let nil =
    { extract =
        (function
          | [] -> Some ()
          | _ -> None)
    ; make = (fun () -> [])
    }

  let cons =
    { extract =
        (function
          | x :: xs -> Some (x, xs)
          | [] -> None)
    ; make = (fun (x, xs) -> x :: xs)
    }
end
