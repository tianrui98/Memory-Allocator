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

open Util
open Allocator

(******************************************************)
(* A doubly linked list parameterised by an allocator *)
(******************************************************)

module DoublyLinkedList (A: Allocator) = struct
    open A
    
    type dll_node = ptr
    
    (* key, value, prev, next *)
   let mk_node heap i s =
      let p = (alloc heap 4) in
      (assign_int heap p 0 i;
       assign_string heap p 1 s;
       assign_ptr heap p 2 (null heap); 
       assign_ptr heap p 3 (null heap);
       p) 
       
    let prev heap (n : dll_node) = deref_as_ptr heap n 2

    let next heap (n : dll_node) = deref_as_ptr heap n 3
    
    let int_value heap (n : dll_node) = deref_as_int heap n 0
    
    let string_value heap (n : dll_node) = deref_as_string heap n 1
    
    (* n1, n2, n3 *)
    let insert_after heap (n1 : dll_node) (n2 : dll_node) = 
      let n3 = next heap n1 in    
      (if is_null heap n3
       then ()
       else assign_ptr heap n3 2 n2);
       (* Set n3 as next of n2, n2 as next of n1, n1 as prev of n2 *)
      assign_ptr heap n2 3 n3; 
      assign_ptr heap n1 3 n2;
      assign_ptr heap n2 2 n1
    
    (* Should free the corresponding memory *)  
    let remove heap n =
      let nxt = next heap n in
      let pre = prev heap n in
      (if is_null heap pre
       then ()
       else assign_ptr heap pre 3 nxt);
      (if is_null heap nxt
       then ()
       else assign_ptr heap nxt 2 pre);
      free heap n 4

    let print_from_node heap n =
      let print_node n =
        Printf.printf "(%d, %s)" (int_value heap n) (string_value heap n);
        if is_null heap (next heap n)
        then ()
        else Printf.printf "; "
      in Printf.printf "[";
      let rec walk i =
        if is_null heap i then () else
          (print_node i;
           walk (next heap i))   
      in walk n;
      Printf.printf "]"     
      
end

(* A test for print_from_node *)
(* let heap = AllocatorImpl.make_heap 20 in
   let n1 = mk_node heap 1 "a" 
   and n2 = mk_node heap 2 "b"
   and n3 = mk_node heap 3 "c" in
   insert_after heap n1 n2;
   insert_after heap n2 n3;
   print_from_node heap n1 *)

(**********************************************)
(*              Testing DLL                   *)
(**********************************************)

open AllocatorImpl

(* Concrete allocator *)
module DLLImpl = DoublyLinkedList(AllocatorImpl)

open DLLImpl

let%test "basic node manipulation" = 
  let heap = AllocatorImpl.make_heap 10 in
  let n1 = mk_node heap 1 "a" 
  and n2 = mk_node heap 2 "b" in
  insert_after heap n1 n2;
  let n = next heap n1 in
  let i = int_value heap n in
  let s = string_value heap n in
  i = 2 && s = "b"

let%test "basic node manipulation: check other node" = 
  let heap = AllocatorImpl.make_heap 10 in
  let n1 = mk_node heap 1 "a" 
  and n2 = mk_node heap 2 "b" in
  insert_after heap n1 n2;
  let n = prev heap n2 in
  let i = int_value heap n in
  let s = string_value heap n in
  i = 1 && s = "a"

(************************************************************************)
(*        TODO: Add more tests for different DLL operations             *)
(************************************************************************)

let%test "basic node manipulation: prev and next when there
are more nodes" = 
  let heap = AllocatorImpl.make_heap 20 in
  let n1 = mk_node heap 1 "a" 
  and n2 = mk_node heap 2 "b"
  and n3 = mk_node heap 3 "c" in
  insert_after heap n1 n2;
  insert_after heap n2 n3;
  let first = prev heap n2 
  and last = next heap n2 in
  let i_first = int_value heap first 
  and s_first = string_value heap first 
  and i_last = int_value heap last 
  and s_last = string_value heap last
  in i_first = 1 && s_first = "a" && i_last = 3 && s_last = "c"

let%test "basic node manipulation: prev and next when 
there is only one node in the list" = 
  let heap = AllocatorImpl.make_heap 10 in
  let n1 = mk_node heap 1 "a" in
  let is_null = AllocatorImpl.is_null in 
  is_null heap (next heap n1) && is_null heap (prev heap n1)

let%test "basic node manipulation: insert before when 
the list is out of memory" = 
  let heap = AllocatorImpl.make_heap 1 in
  try let _ = mk_node heap 1 "a" in false
  with Failure _ -> true
