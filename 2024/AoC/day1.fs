module day1

open System.Collections;
open System.Collections.Generic
open System
open System.IO;
open utils

let f = File.ReadAllLines("data/day1.txt")

f |> List.map (fun s -> s.Split ' ')