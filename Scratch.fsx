open System
// OCaml Style
//type ('a, 'b) T = | Ok of 'a | Error of 'b
type T<'a, 'b> = | Ok of 'a | Error of 'b
let (x : T<int, string>) = Ok 1
let (x' : (int, string) T) = Error "1"

//type 'a Expr = | Base of 'a | Other of bool
type Expr<'a> = | Base of 'a | Other of bool

module Test =
  type X = { SessionId : string; Time : DateTime }

// OCaml Allows optional arguments for functions, including defaults.
//let testFunc a ?b = a + (defaultArg b "")
//let testFunc a ?(b="") = a + b
