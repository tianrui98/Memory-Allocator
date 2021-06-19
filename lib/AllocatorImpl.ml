(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2020 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open Allocator
open Util
open ArrayUtil

(**********************************************)
(*   A concrete implementation of allocator   *)
(**********************************************)

module AllocatorImpl : Allocator = struct

  type ptr = int option

  type value = 
    | Free
    | Init
    | I of int 
    | S of string
    | P of ptr

  type heap = value array

  let make_heap sz =
    Array.make sz Free

  let null h = None

  let is_null h p =
    p == None

  let alloc h size =
    let len = Array.length h in
    let counter h i size =
      (* counter looks for a chunk of Free memory of a certain size *)
      let rec count a' =
        if (a' < i + size) && (a' < len)
        then (match h.(a') with
            | Free -> count (a' + 1)
            | _ -> (a' + 1)) (* returns to find; looks for free memory
                              * from next index to not repeat traversal *)
        else i (* returns index of first free slot *)
      in count i
    in
    let rec find h a =
      (* find looks for the first available Free space *)
      if a < len
      then (match h.(a) with
          | Free ->
            (let res = counter h a size in
             if res = a 
             then (for i = a to a + size - 1 do
                     if i < len 
                     then h.(i) <- Init
                     else error "Out-Of-Memory"           
                   done; Some a) (* returns pointer to first free index *)
             else find h res)
          | _ -> find h (a + 1))
      else error "Out-Of-Memory"
    in find h 0

  let free h p size = 
    let pi = (get_exn p) in
    if pi + size > Array.length h 
    then error "Exceed maximal heap capacity!"
    else 
      for i = pi to pi + size - 1 do
        h.(i) <- Free
      done

  let deref_as_ptr h p o =
    let pi = (get_exn p) in
    if pi + o >= Array.length h 
    then error "Index out of bounds!"
    else 
      let i = (get_exn p) + o in
      let poss_ptr = h.(i) in
      match poss_ptr with
      | P p' -> p'
      | _ -> error "Not a pointer!"

  let deref_as_int h p o =
    let pi = (get_exn p) in
    if pi + o >= Array.length h 
    then error "Index out of bounds!"
    else 
      let i = (get_exn p) + o in
      let poss_int = h.(i) in
      match poss_int with
      | I i' -> i'
      | _ -> error "Not an integer!"

  let deref_as_string h p o =
    let pi = (get_exn p) in
    if pi + o >= Array.length h 
    then error "Index out of bounds!"
    else 
      let i = (get_exn p) + o in
      let poss_str = h.(i) in
      match poss_str with
      | S s' -> s'
      | _ -> error "Not a string!"

  let assign_ptr h p o v =
    let pi = (get_exn p) in
    if pi + o >= Array.length h 
    then error "Index out of bounds!"
    else 
      match h.(pi + o) with
      | Free -> error "Memory not allocated!"
      | Init -> h.(pi + o) <- P v
      | P p' -> h.(pi + o) <- P v
      | _ -> error "Memory is already occupied!"

  let assign_int h p o v =
    let pi = (get_exn p) in
    if pi + o >= Array.length h 
    then error "Index out of bounds!"
    else 
      match h.(pi + o) with 
      | Free -> error "Memory not allocated!"
      | Init -> h.(pi + o) <- I v
      | _ -> error "Memory is already occupied!"

  let assign_string h p o v =
    let pi = (get_exn p) in
    if pi + o >= Array.length h 
    then error "Index out of bounds!"
    else 
      match h.(pi + o) with 
      | Free -> error "Memory not allocated!"
      | Init -> h.(pi + o) <-  S v
      | _ -> error "Memory is already occupied!"

end

(**********************************************)
(*              Testing allocator             *)
(**********************************************)

(* Positive test *)

let%test "int ptr preservation" = 
  let open AllocatorImpl in
  let hp = make_heap 1000 in
  let ptr = alloc hp 2 in
  assign_int hp ptr 0 42;
  assign_int hp ptr 1 12;
  let res1 = deref_as_int hp ptr 0 in
  let res2 = deref_as_int hp ptr 1 in
  res1 = 42 && res2 = 12

(* Negative test: notice that the error message is ignored *)

let%test "entire array occupied: full heap failure" = 
  let open AllocatorImpl in
  let hp = make_heap 2 in
  let ptr = alloc hp 2 in
  assign_int hp ptr 0 42;
  assign_int hp ptr 1 12;
  try let _ = alloc hp 1 in false
  with Failure _ -> true 

(************************************************************************)
(* TODO: Add more tests for strings and pointers, and other operations. *)
(************************************************************************)

(* Positive test *)
let%test "String ptr preservation" = 
  let open AllocatorImpl in
  let hp = make_heap 1000 in
  let ptr = alloc hp 2 in
  assign_string hp ptr 0 "a";
  assign_string hp ptr 1 "b";
  let res1 = deref_as_string hp ptr 0 in
  let res2 = deref_as_string hp ptr 1 in
  res1 = "a" && res2 = "b"

let%test "int and string ptr preservation" = 
  let open AllocatorImpl in
  let hp = make_heap 1000 in
  let ptr = alloc hp 3 in
  assign_string hp ptr 0 "a";
  assign_string hp ptr 1 "b";
  assign_int hp ptr 2 1;
  let res1 = deref_as_string hp ptr 0 in
  let res2 = deref_as_string hp ptr 1 in
  let res3 = deref_as_int hp ptr 2 in
  res1 = "a" && res2 = "b" && res3 = 1

let%test "alloc finds adequate size" =
  let open AllocatorImpl in
  let hp = make_heap 20 in
  let ptr1 = alloc hp 10 in
  free hp ptr1 5;
  let ptr2 = alloc hp 7 in
  ptr1 <> ptr2

let%test "alloc more space than there is" = 
  let open AllocatorImpl in 
  let hp = make_heap 1 in 
  try let _ = alloc hp 4 in false
  with Failure _ -> true

(*negative test: assume the user doesn't know how the allocator works *)
let%test "free too much memory than there is in the heap" = 
  let open AllocatorImpl in
  let hp = make_heap 2 in
  let ptr = alloc hp 2 in
  assign_int hp ptr 0 42;
  assign_int hp ptr 1 12;
  try let _ = free hp ptr 3 in false
  with Failure _ -> true 

