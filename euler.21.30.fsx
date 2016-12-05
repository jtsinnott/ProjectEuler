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
(*
A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.
As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.
Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
*)
let p23 () =
  let isAbundant n = n < (divisors n |> Seq.sum)
  let isAbundant' = memoize isAbundant
  seq { 1L..28123L }
  |> Seq.filter (
       fun x ->
         seq { 1L..(x/2L + 1L) }
         |> Seq.forall (fun y -> not (isAbundant y && isAbundant (x-y))))
  |> Seq.sum

p23 ()

(* PROBLEM 24 *)
(*
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
012   021   102   120   201   210
What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*)

(* PROBLEM 25*)
(*
What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
*)
