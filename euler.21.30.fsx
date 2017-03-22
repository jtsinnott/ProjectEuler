#load "Utils.fs"
#load "Seq.fs"
#time

open Euler.Utils
open Euler.Seq
open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

(* https://projecteuler.net/archives *)

(* PROBLEM 21 *)
let p21 () =
  let d n =
    seq {
      for i in 1..(n-1) do if n % i = 0 then yield i
    } |> Seq.sum
  seq {
    for a in [1..9999] do
      let b = d a
      if ((d b) = a && a <> b) then yield a;
  } |> Seq.sum

// p21 ()


// Mutable version is 10x faster.
let p21' () =
  let d n =
    let mutable sum = 1
    for i in 2..(n-1) do
      if n % i = 0 then
        sum <- sum + i
    sum
  let sumAmicable () =
    let mutable sum = 0
    for a in [1..9999] do
      let b = d a
      if ((d b) = a && a <> b) then
        sum <- sum + a
    sum
  sumAmicable ()

// p21' ()

(* PROBLEM 22 *)
let p22 () =
  let scoreName (name : String) =
    name.ToUpper().ToCharArray()
    |> Array.map (fun c -> 1 + Array.findIndex ((=) c) [|'A'..'Z'|])
    |> Array.sum

  let names = (File.ReadAllText "p022_names.txt").Split (',')
  names
  |> Array.map (fun n -> n.Replace("\"", ""))
  |> Array.sort
  |> Array.mapi (fun i name -> ((i + 1) * scoreName name, name))
  |> Array.sumBy fst

// p22 ()

(* PROBLEM 23 *)
let p23 () =
  let isAbundant n = n < Seq.sum (properDivisors n)
  let isAbundant' = memoize isAbundant
  let hasNoAbundantPairs x =
    seq { 1L..(x/2L + 1L) }
    |> Seq.filter (fun y -> (isAbundant' y && isAbundant' (x-y)))
    |> Seq.isEmpty
  seq { 1L..28123L }
  |> Seq.filter hasNoAbundantPairs
  |> Seq.sum

// p23 ()

(* PROBLEM 24 *)
(*
A permutation is an ordered arrangement of objects. For example, 3124 is one 
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations 
are listed numerically or alphabetically, we call it lexicographic order. The 
lexicographic permutations of 0, 1 and 2 are:
012   021   102   120   201   210
What is the millionth lexicographic permutation of the digits 
0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*)
let p24 () =
  let rec permute digits =
    seq {
      for i in 0..9 do
        if i <> not then
          i

    }

(* PROBLEM 25*)
(*
What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
*)
