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

module Iso : sig
  type ('a, 'b) t = ('a, 'b) iso =
    { ltor : 'a -> 'b
    ; rtol : 'b -> 'a
    }

  val ltor : ('a, 'b) iso -> 'a -> 'b
  val rtol : ('a, 'b) iso -> 'b -> 'a
  val map : ('a, 'b) iso -> ('b -> 'b) -> 'a -> 'a
  val compose : ('a, 'b) iso -> ('b, 'c) iso -> ('a, 'c) iso
  val ( @@ ) : ('a, 'b) iso -> ('b, 'c) iso -> ('a, 'c) iso
  val product : ('a, 'b) iso -> ('c, 'd) iso -> ('a * 'c, 'b * 'd) iso
  val ( <*> ) : ('a, 'b) iso -> ('c, 'd) iso -> ('a * 'c, 'b * 'd) iso
  val assoc : (('a * 'b) * 'c, 'a * ('b * 'c)) iso
end

module Lens : sig
  type ('a, 'b) t = ('a, 'b) lens =
    { get : 'a -> 'b
    ; put : 'b -> 'a -> 'a
    }

  val get : ('a, 'b) lens -> 'a -> 'b
  val put : ('a, 'b) lens -> 'b -> 'a -> 'a
  val map : ('a, 'b) lens -> ('b -> 'b) -> 'a -> 'a
  val compose : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens
  val ( @@ ) : ('a, 'b) lens -> ('b, 'c) lens -> ('a, 'c) lens
  val product : ('a, 'b) lens -> ('c, 'd) lens -> ('a * 'c, 'b * 'd) lens
  val ( <*> ) : ('a, 'b) lens -> ('c, 'd) lens -> ('a * 'c, 'b * 'd) lens
  val id : ('a, 'a) lens
  val fst : ('a * 'b, 'a) lens
  val snd : ('a * 'b, 'b) lens
  val is_value : ?eq:('a -> 'a -> bool) -> 'a -> ('a, bool) lens
  val is : ('a, 'b) prism -> 'b -> ('a, bool) lens
end

module Prism : sig
  type ('a, 'b) t = ('a, 'b) prism =
    { extract : 'a -> 'b option
    ; make : 'b -> 'a
    }

  val extract : ('a, 'b) prism -> 'a -> 'b option
  val make : ('a, 'b) prism -> 'b -> 'a
  val map : ('a, 'b) prism -> ('b -> 'b) -> 'a -> 'a
  val compose : ('a, 'b) prism -> ('b, 'c) prism -> ('a, 'c) prism
  val product : ('a, 'b) prism -> ('c, 'd) prism -> ('a * 'c, 'b * 'd) prism
  val id : ('a, 'a) prism
  val satisfy : ('a -> bool) -> ('a, 'a) prism
  val is : 'a -> ('a, 'a) prism
  val none : ('a option, unit) prism
  val some : ('a option, 'a) prism
  val nil : ('a list, unit) prism
  val cons : ('a list, 'a * 'a list) prism
end
