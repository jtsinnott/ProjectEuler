﻿namespace Euler

open System

module Utils =

  let memoize f =
    let cache = new System.Collections.Generic.Dictionary<_,_>()
    fun x ->
      if cache.ContainsKey(x) then cache.[x]
      else
        let res = f x
        cache.[x] <- res
        res

  let sqrt' (x : int64) = int64(sqrt(float(x)))

  let divisors x =
    seq { 1L..sqrt'(x) }
    |> Seq.filter (fun i -> x % i = 0L)
    |> Seq.collect (fun i -> [i; x/i])
    |> Seq.distinct

  let properDivisors x = divisors x |> Seq.filter ((<>) x)

  let isPrime x =
    if x = 2L || x = 3L then true
    else
      seq {
        for i in 2L..(sqrt'(x) + 2L) do
          if x % i = 0L then
            yield i }
      |> Seq.isEmpty

  // Classically recursive but much slower than version above.
  let isPrime' x =
    let rec check i =
      i > x / 2 || (x % i <> 0 && check (i + 1))
    check 2

  let isPalindrome (x : string) =
    let chars = x.ToCharArray()
    chars = (Array.rev chars)

  let charToInt (c : Char) = snd (Int32.TryParse (string c))
  let product charr = charr |> Array.map charToInt |> Array.reduce (*)
