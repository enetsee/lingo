type break =
  | Flat
  | Fit
  | Hard of int

type optional_sep =
  | Never
  | On_break
  | Always

type position =
  | Ends_line
  | Starts_line

type sep =
  { sep_kind : Kind.t
  ; text : string
  ; leading : optional_sep
  ; trailing : optional_sep
  ; position : position
  }

type frame =
  | Plain
  | Delimited of
      { open_ : Kind.t
      ; close : Kind.t
      ; sep : sep option
      ; open_space : bool
      ; pad : bool
      }
  | Separated of sep

type slot =
  { kinds : Kind.t array
  ; repeats : bool
  ; before : break
  ; between : break
  ; space : bool
  }

type rule =
  { name : string
  ; kind : Kind.t
  ; frame : frame
  ; slots : slot array
  ; body : break
  ; inner : break
  ; indent : int
  }

type trivia =
  | Reformat
  | Preserve

type side =
  | Hug
  | Free
  | Space

type token =
  { space_before : side
  ; space_after : side
  ; trivia : trivia option
  }

let not_a_token = { space_before = Free; space_after = Free; trivia = None }

type t =
  { rules : rule array
  ; of_kind : int array
  ; tokens : token option array
  }
