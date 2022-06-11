(** Unicorn provides a small algebra of seven combinators to define GUI applications:

    - [empty] for when you don't want anything drawn
    - [a & b] when you want to display two or more widgets
    - [iso i w] to adapt an existing widget [w] to a different type (similar to "map")
    - [on lens w] when you want to edit a product/tuple/record
    - [into prism w] for pattern matching over sum types
    - [stateful s w] to hide the internal state of the widget [w]
    - [dynamic] for complete control over the widget's position, movement, creation, history, etc.

  These combinators are the glue to assemble complex applications out of smaller widgets.
  The [jsoo] ([js_of_ocaml]) backend additionnally provides the primitive widgets and HTML support.
*)

type !'a t
(** The type of a widget that can render and update values of type ['a]. *)

val run : ?id:string -> 'a t -> 'a -> unit
(** [run w x] renders the widget [w] with the initial state [x], then
    proceed to update it in reaction to user events. It's the main loop
    that drives the GUI application, so you probably want to call it only once.

    - If [?id] is specified, the widget renders in the HTML node with
      the corresponding [id] attribute.
      Otherwise, the widget is appended to the HTML body of the webpage.
*)

(** {1 Algebra} *)

val empty : 'a t
(** The [empty] widget, invisible and ineffective. *)

val ( & ) : 'a t -> 'a t -> 'a t
(** [a & b] displays both widgets [a] and [b] side by side.

    - identity: [∀ w. empty & w == w == w & empty]
    - associative: [∀ a b c. a & (b & c) == a & b & c == (a & b) & c]
    - NOT commutative, as [b & a] displays the widgets in a different order.
*)

open Optic
(** The polymorphic ['a] in an ['a t] widget is invariant: It is consummed when rendering,
    but also produced in reaction to user events. As such, we always need pair of functions
    to "map" over it: *)

val iso : ('a, 'b) iso -> 'b t -> 'a t
(** [iso i w] maps the isomorphism [i] over the widget [w].

    - identities: [∀ i. iso i empty == empty] and [∀ w. map Iso.id w == w]
    - composition: [∀ i j w. iso i (iso j w) == map (Iso.compose i j) w]
    - distributive: [∀ i a b. iso i a & iso i b == iso i (a & b)]
*)

val on : ('a, 'b) lens -> 'b t -> 'a t
(** [on lens w] zooms on a sub-field of the state ['a] with the [lens]
    and renders/updates it with the widget [w].

    - identities: [∀ lens. on lens empty == empty] and [∀ w. on Lens.id w == w]
    - composition: [∀ l1 l2 w. on l1 (on l2 w) == on (Lens.compose l1 l2) w]
    - distributive: [∀ lens a b. on lens a & on lens b == on lens (a & b)]
*)

val into : ('a, 'b) prism -> 'b t -> 'a t
(** [into prism w] pattern matches the state ['a] with the [prism], to render
    the widget [w]. If the prism fails, then it renders [empty].

    - identities: [∀ prism. into prism empty == empty] and [∀ w. into Prism.id w == w]
    - composition: [∀ p1 p2 w. into p1 (on p2 w) == into (Prism.compose p1 p2) w]
    - distributive: [∀ prism a b. into prism a & into prism b == into prism (a & b)]
*)

val case : ('a, 'b) prism -> 'b t -> 'a t
(** [case prism w] is the same as [into prism w], but the internal state of
    the widget [w] is reset when the prism is not satisfied.

    - definition: [∀ p w. into p w == stateful w (case Prism.(product id p) dynamic)]
*)

val stateful : 's -> ('s * 'a) t -> 'a t
(** [stateful s w] encapsulates the internal state of the widget [w]
    with an initial value [s].

    - identities: [∀ s. stateful s empty == empty]
              and [∀ s w. stateful s (on Lens.snd w) == w]
    - composition: [∀ s1 s2 w. stateful s1 (stateful s2 w) == stateful (s1, s2) (iso Iso.assoc w)]
    - distributive: [∀ s a b. stateful s a & b == stateful s (a & on Lens.snd b)]
                and [∀ s a b. a & stateful s b == stateful s (on Lens.snd a & b)]
    - commutative: [∀ i s w. iso i (stateful s w) = stateful s (iso (Iso.product id i) w)]
               and same for lenses and prisms.

    However, internal state is not shared: [stateful s a & stateful s b =/= stateful s (a & b)].
*)

val dynamic : ('a t * 'a) t
(** [dynamic] renders and updates a dynamic widget coming from the state.

    - [∀ w. w == stateful w dynamic]
*)

(** {2 Useful combinators} *)

(** The following are defined from the previous combinators. *)

val of_list : 'a t list -> 'a t
(** [of_list ws] displays the static list of widget [ws]. *)

val list : 'a t -> 'a list t
(** [list w] displays a dynamic list, using the widget [w] for each element. *)

val ( <*> ) : 'a t -> 'b t -> ('a * 'b) t
(** [a <*> b] is the product of the two widgets [a] and [b]. *)

val cond : ('a -> bool) -> 'a t -> 'a t
(** [cond p w] renders the widget [w] when the predicate [p] is satisfied.

    - identities: [∀ p. cond p empty == empty]
              and [∀ w. cond (fun _ -> true) w == w]
              and [∀ w. cond (fun _ -> false) w == empty]
    - composition: [∀ p q w.  cond p (cond q w) == cond (fun x -> p x && q x) w]
    - distributive: [∀ p a b. cond p (a & b) == cond p a & cond p b]
*)

val ifte : ('a -> bool) -> 'a t -> 'a t -> 'a t
(** [ifte p if_true if_false] renders the widget [if_true] when [p] is satisfied,
    otherwise [if_false]. *)

val cond_forget : ('a -> bool) -> 'a t -> 'a t
(** Same as [cond], but the internal state is lost when the predicate
    is unsatisfied (see {!case}). *)

val stateful_by : ('a -> 's) -> ('s * 'a) t -> 'a t
(** [stateful_by f w] is the same as [stateful], but the initial value is computed
    from the current state. *)

(** {2 Recursive definitions} *)

(** As OCaml is a strict language, recursive definitions need special support as
    this would fail to terminate:

      {[let rec list w = into Prism.cons (w <*> list w)]}

    and one should rather delay slightly the recursion:

      {[let rec list w = into Prism.cons (w <*> apply list w)]}
*)

val of_lazy : 'a t Lazy.t -> 'a t
(** [of_lazy w] delays the lazy widget [w], such that [of_lazy (lazy w) == w] *)

val apply : ('a -> 'b t) -> 'a -> 'b t
(** [apply f x] is [of_lazy (lazy (f x))]. *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] uses [of_lazy] to simplify the definition of recursive traversals. *)

(** {1 HTML primitive widgets} *)

val str : string -> 'a t
(** [str s] displays the constant string [s] as HTML. *)

val text : ('a -> string) -> 'a t
(** [text f] renders a dynamic string using the function [f] on the current state. *)

val checkbox : bool t
(** [checkbox] is a primitive widget to render and edit a boolean. *)

val input_int : int t
(** [input_int] is a primitive widget to render and edit an integer as a text input. *)

val input_string : string t
(** [input_string] is a primitive widget to render and edit a single-line string. *)

val button : 'a t -> 'a t
(** [button w] is an HTML button using the widget [w] for its contents.
    The reaction when the button is clicked can be specified with {!E.click}:

      {[button (str "Click" & E.click (fun old_state -> new_state))]}
*)

module H : sig
  (** HTML elements and nodes, like div, span, etc. *)

  (** We don't check that the HTML elements are used correctly!

      By convention, when the following functions takes an ['a t list] for their
      children, then they must only be passed attributes (and not children nodes).
   *)

  val a : 'a t -> 'a t
  val div : 'a t -> 'a t
  val h1 : 'a t -> 'a t
  val h2 : 'a t -> 'a t
  val h3 : 'a t -> 'a t
  val h4 : 'a t -> 'a t
  val h5 : 'a t -> 'a t
  val h6 : 'a t -> 'a t
  val hr : 'a t list -> 'a t
  val label : 'a t -> 'a t
  val li : 'a t -> 'a t
  val p : 'a t -> 'a t
  val pre : 'a t -> 'a t
  val span : 'a t -> 'a t
  val ul : 'a t -> 'a t
  val section : 'a t -> 'a t
  val header : 'a t -> 'a t
  val footer : 'a t -> 'a t
  val button : 'a t -> 'a t
  val input_string : ('a, string) lens -> 'a t list -> 'a t
  val checkbox : ('a, bool) lens -> 'a t list -> 'a t

  (** {2 Unsafe}
      Consider submitting a PR if you find yourself using this!
  *)

  val make : string -> 'a t -> 'a t
  (** [make name children] creates a new HTML node with tag [name] and [children]. *)

  val leaf : string -> 'a t list -> 'a t
  (** [leaf name attrs] creates a new HTML element with tag [name] and a list
      of optional attributes (no children nodes). *)
end

module A : sig
  (** HTML attributes and properties. *)

  (** We don't differentiate betwen children nodes and attributes/properties.
      The attributes specified inside an HTML node attach to their closest parent node:

        {[H.div ( ... & A.class_ (fun _ -> "foo") & ... )]}

      will produce the HTML [<div class="foo">...</div>]
  *)

  val id : ('a -> string) -> 'a t
  val class_ : ('a -> string) -> 'a t
  val style : ('a -> string) -> 'a t
  val for_ : ('a -> string) -> 'a t
  val disabled : ('a -> bool) -> 'a t
  val tabindex : ('a -> int) -> 'a t
  val placeholder : ('a -> string) -> 'a t
  val autofocus : ('a -> bool) -> 'a t

  (** {2 Unsafe}
      Consider submitting a PR if you find yourself using this!
   *)

  val make_string : string -> ('a -> string) -> 'a t
  (** [make_string attr value] attaches an attribute/property to its HTML parent node. *)

  val make_int : string -> ('a -> int) -> 'a t
  (** [make_int attr value] attaches an integer attribute/property to its HTML parent node. *)

  val make_bool : string -> ('a -> bool) -> 'a t
  (** [make_string attr value] attaches a boolean attribute/property to its HTML parent node. *)
end

module E : sig
  (** HTML events. *)

  (** Just like attributes, events are specified inside the HTML node children and attach
      to their closest parent. *)

  open Js_of_ocaml

  val click : ('a -> 'a) -> 'a t
  (** [click fn] invokes the callback [fn] when its HTML parent node is clicked.

      - [∀ f g. click f & click g == click (fun x -> g (f x))]
      - [∀ i f. iso i (click f) == click (Iso.map i f)] and same for lenses and prisms
  *)

  val doubleclick : ('a -> 'a) -> 'a t
  val keydown : (int -> 'a -> 'a) -> 'a t
  val blur : ('a -> 'a) -> 'a t

  (** {2 Lifecycle} *)

  val init : (Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> 'a -> 'a) -> 'a t
  (** [init fn] calls [fn] once on the creation of the HTML widget. *)

  val init_focus : unit -> 'a t
  (** [init_focus ()] focuses the HTML parent element on creation. *)

  (** {2 Animations} *)

  val now : (float -> 'a -> 'a) -> 'a t
  (** [now fn] invokes the callback [fn] on every frame with the current time and state. *)

  val dt : (float -> 'a -> 'a) -> 'a t
  (** [dt fn] invokes the callback [fn] on every frame with the delta time and state. *)

  (** {2 Unsafe}
      Consider submitting a PR if you find yourself using this!
  *)

  val make : (_ #Dom.event Js.t as 'e) Dom_html.Event.typ -> ('e -> 'a -> 'a) -> 'a t
  (** [make ev callback] attaches a new event handler to the HTML parent node. *)
end
