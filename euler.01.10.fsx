#load "utils.fs"
#load "Seq.fs"
#time

open Euler.Utils
open Euler.Seq
open System

(* https://projecteuler.net/archives *)

(* PROBLEM 1 *)
let p1 () =
  [|1..999|]
  |> Seq.filter (fun i -> i % 3 = 0 || i % 5 = 0)
  |> Seq.sum

let p1' () =
  seq { for i in 1..999 do
          if i % 3 = 0 || i % 5 = 0 then
            yield i }
  |> Seq.sum

//p1 ()
//p1' ()

(* PROBLEM 2 *)
let p2 () =
  let rec fib (a : int) (b : int) = seq {
    yield a
    yield! (fib b (a + b))
  }

  fib 1 1
  |> Seq.takeWhile (fun x -> x < 4000000)
  |> Seq.where (fun x -> x % 2 = 0)
  |> Seq.sum

// Tree-recursive version
let p2' () =
  let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)
  seq { 1..100 } |> Seq.map fib
//p2 ()

(* PROBLEM 3 *)
let p3 () =
  let n = 600851475143L
  seq { 2L..(sqrt'(n) + 2L) }
  |> Seq.filter (fun i -> n % i = 0L) // Filter by mod first.
  |> Seq.filter isPrime               // Then by isPrime.
  |> Seq.max

//p3 ()

(* PROBLEM 4 *)
let p4 () =
  seq { for i = 999 downto 100 do // Count down to get result faster.
          for j = 999 downto 100 do
            yield i * j }
  |> Seq.filter (string >> isPalindrome)
  |> Seq.take 1
  |> Seq.exactlyOne

//p4 ()

(* PROBLEM 5 *)
let p5 () =
  let nums = seq { 2..20 }
  let isDivisibleByAllNums x = nums |> Seq.forall (fun y -> x % y = 0)
  // Here we increment in steps of <maxnum>
  let max = Seq.max nums
  Seq.unfold (fun x -> Some(x, x + max)) max
  |> Seq.find isDivisibleByAllNums

//p5 ()

(* PROBLEM 6 *)
let p6 () =
  let sumOfSquares x = x |> Seq.sumBy (fun x -> x*x)
  let squareOfSums x = let sumX = (Seq.sum x) in sumX * sumX
  let first100N = seq { 1..100 }
  abs((sumOfSquares first100N) - (squareOfSums first100N))

//p6 ()

(* PROBLEM 7 *)
let p7 () =
  Seq.unfold (fun i -> Some(i, i+1L)) 1L
  |> Seq.filter isPrime
  |> Seq.item 10000 // item is 0 based index

//p7 ()

(* PROBLEM 8 *)
let p8 () =
  System.IO.File.ReadAllText("p8.txt").ToCharArray()
  |> Seq.windowed 13
  |> Seq.map product
  |> Seq.max

//p8 ()

(* PROBLEM 9 *)
let p9 () =
  seq { for a = 1 to 1000 do
          for b = 1 to (1000-a) do
            let c = 1000 - a - b
            if a * a + b * b = c * c then
              yield (a, b, c) }
  |> Seq.distinctBy (fun (a, b, c) -> a + b + c)
  |> Seq.map (fun (a, b, c) -> a * b * c)
  |> List.ofSeq

//p9 ()

(* PROBLEM 10 *)
let p10 () =
  seq { 2L..(2000000L-1L) }
  |> Seq.filter isPrime
  |> Seq.sum

//p10 ()
