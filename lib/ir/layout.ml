type break =
  | Flat
  | Fit
  | Hard of int

type trailing =
  | Never
  | On_break
  | Always

type sep =
  { sep_kind : Kind.t
  ; text : string
  ; trailing : trailing
  }

type frame =
  | Plain
  | Delimited of
      { open_ : Kind.t
      ; close : Kind.t
      ; sep : sep option
      }
  | Separated of sep

type slot =
  { kinds : Kind.t array
  ; repeats : bool
  ; before : break
  ; between : break
  }

type rule =
  { name : string
  ; kind : Kind.t
  ; frame : frame
  ; slots : slot array
  ; body : break
  ; inner : break
  ; indent : int
  ; edge_before : bool option
  ; edge_after : bool option
  }

type trivia =
  | Reformat
  | Preserve

type token =
  { space_before : bool
  ; space_after : bool
  ; trivia : trivia option
  }

let not_a_token = { space_before = true; space_after = true; trivia = None }

type t =
  { rules : rule array
  ; of_kind : int array
  ; tokens : token option array
  }
