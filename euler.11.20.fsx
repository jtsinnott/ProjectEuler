#load "utils.fs"
#load "Seq.fs"
#time

open Euler.Utils
open Euler.Seq
open System
open System.Text.RegularExpressions

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

(* https://projecteuler.net/archives *)

(* PROBLEM 11 *)
type Direction = Down | Right | DownRight | DownLeft
let p11 () =
  let grid =
    let digits = System.IO.File.ReadAllText("p11.txt")
    digits.Split('\n')
    |> Array.map (fun l -> l.Split(' ') |> Array.map int)

  let max_i, max_j = grid.Length - 1, grid.[0].Length - 1

  let gridVec i j n d =
    let up i n = [| i..(-1)..(i - (n-1)) |]
    let dn i n = [| i..(i + (n-1)) |]
    let lt j n = [| j..(-1)..(j - (n-1)) |]
    let rt j n = [| j..(j + (n-1)) |]
    let stay (x : int) n = Array.replicate n x
    let f (i, j) = i <= max_i && i > 0 && j <= max_j && j > 0
    let vec = function
      | Down      -> Array.zip (dn i n) (stay j n)
      | Right     -> Array.zip (stay i n) (rt j n)
      | DownRight -> Array.zip (dn i n) (rt j n)
      | DownLeft  -> Array.zip (dn i n) (lt j n)
    d |> vec |> Array.filter f

  let gridVecProd i j n d =
    gridVec i j n d
    |> Array.map (fun (i,j) -> grid.[i].[j])
    |> (fun xs -> if (Array.isEmpty xs) then 0 else Array.reduce (*) xs)

  seq {
    for i in 0..max_i do
      for j in 0..max_j do
        for d in [Down; Right; DownLeft; DownRight] do
          yield gridVecProd i j 4 d
  } |> Seq.max

// p11 ()

(* PROBLEM 12 *)
let p12 () =
  let triangleNum n = [1L..n] |> Seq.sum
  let naturalNums = Seq.unfold (fun i -> Some (triangleNum i, i+1L)) 1L

  naturalNums
  |> Seq.map (fun t -> (t, Seq.length (divisors t)))
  |> Seq.find (fun (_, l) -> l > 500)

//p12 ()

(* PROBLEM 13 *)
let p13() =
  System.IO.File.ReadAllLines("p13.txt")
  |> Array.map (fun x -> Int64.Parse (x.Substring(0, 11)))
  |> Array.sum
  |> (fun x -> (string x).Substring(0, 10))

//p13 ()

(* PROBLEM 14 *)
let p14 () =
  let nextCollatz n = if n % 2L = 0L then n/2L else 3L*n + 1L
  let rec collatzSeqLength = function
    | 1L -> 1L
    | n  -> 1L + (collatzSeqLength (nextCollatz n)) // Not tail recursive.
  seq { 1L..999999L } |> Seq.maxBy collatzSeqLength

//p14 ()

(* PROBLEM 15 *)
let p15 () =
  let rec fac n = if n <= 1I then 1I else fac(n-1I) * n
  // combination (n sub k) = n! / (k! * (n-k)!)
  let combo n k = (fac n) / (fac k * (fac (n-k)))
  combo 40I 20I

//p15 ()

(* PROBLEM 16 *)
let p16 () =
  (string (pown (bigint 2) 1000)).ToCharArray()
  |> Array.sumBy (string >> Int32.TryParse >> snd)

//p16 ()

(* PROBLEM 17 *)
let p17 () =
  let rec numWord = function
    | 0  -> "" | 1 -> "one" | 2 -> "two" | 3 -> "three" | 4 -> "four"
    | 5  -> "five" | 6 -> "six" | 7 -> "seven" | 8 -> "eight" | 9 -> "nine"
    | 11 -> "eleven" | 12 -> "twelve" | 13 -> "thirteen" | 14 -> "fourteen"
    | 15 -> "fifteen" | 16 -> "sixteen" | 17 -> "seventeen" | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 10 -> "ten" | 20 -> "twenty" | 30 -> "thirty" | 40 -> "forty"
    | 50 -> "fifty" | 60 -> "sixty" | 70 -> "seventy" | 80 -> "eighty"
    | 90 -> "ninety"
    | 1000 -> "one thousand"
    | n when n % 100 = 0  ->
      let hundreds = numWord (n / 100)
      sprintf "%s hundred" hundreds
    | n when n / 100 >= 1 ->
      let hundreds = numWord (n / 100)
      let rest = numWord (n % 100)
      sprintf "%s hundred and %s" hundreds rest
    | n when n / 10 > 1 ->
      let tens = numWord (n - (n % 10))
      let ones = numWord (n % 10)
      sprintf "%s-%s" tens ones
    | n -> sprintf "unknown number %d" n

  let cleanWord word = Regex.Replace (word, "[\W-]", "")

  [1..1000] |> List.sumBy (numWord >> cleanWord >> String.length)

//p17 ()

(* PROBLEM 18 *)
let p18 () =
  let nums =
    IO.File.ReadAllLines ("p18.txt")
    |> Array.map (fun l -> l.Split (' ') |> Array.map int)
    |> List.ofArray

  let rec sumPaths (acc : int list) (nums : int [] list) =
    match nums with
    | []        -> acc
    | h :: rest ->
      let tmp = Array.create (h.Length) 0
      acc |> List.iteri (fun i e ->
        h |> Array.iteri (fun j f ->
          if i = j || (i + 1) = j then
            tmp.[j] <- max tmp.[j] (e + f)))
      sumPaths (List.ofArray tmp) rest

  (sumPaths [0] nums) |> List.max

//p18 ()

(* PROBLEM 19 *)
let p19 () =
  let daysInMonth year month =
    let isLeapYear = year % 4 = 0 && (year % 100 <> 0 || year % 400 = 0)
    match month with
    | 2              -> if isLeapYear then 29 else 28
    // Sep, Apr, Jun, Nov
    | 9 | 4 | 6 | 11 -> 30
    | _              -> 31

  let isSunday daysSinceJan_1_1900 = (daysSinceJan_1_1900 % 7) = 6

  seq {
    for y in 1900..2000 do
      let daysInMonth' = daysInMonth y
      for m in 1..12 do
        yield (y, m, daysInMonth' m, 0)
  }
  |> Seq.skip 1 // We'll seed Jan 1900 below.
  |> Seq.scan (
      fun (_, _, dm, dt) (y, m, dm', _) -> (y, m, dm', dm + dt)) (1900,1,31,0)
  |> Seq.filter (fun (y, _, _, dt) -> y > 1900 && isSunday dt)
  |> Seq.length

//p19 ()

(* PROBLEM 20 *)
let p20 () =
  let rec fac (n : bigint) = if n = 0I then 1I else n * fac (n - 1I)
  (fac 100I |> string).ToCharArray()
  |> Array.map (string >> Int32.TryParse >> snd)
  |> Array.sum

//p20 ()
