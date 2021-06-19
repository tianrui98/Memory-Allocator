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
open SinglyLinkedList
open Queue

(******************************************************)
(*        A queue based on singly linked lists        *)
(******************************************************)

(* A queue interface *)
module type Queue = sig
  type t
  val mk_queue : int -> t
  val is_empty : t -> bool
  val is_full :  t -> bool
  val enqueue :  t -> (int * string) -> unit
  val dequeue :  t -> (int * string) option
  val queue_to_list : t -> (int * string) list
end

module HeapSLLQueue (A: Allocator) = struct
(* module HeapSLLQueue (A: Allocator) : Queue = struct *)
  module SLL = SinglyLinkedList(A)
  open A
  open SLL

  (* Opaque queue type *)
  type t = {
    elems: heap;
    head: ptr;
    tail: ptr;
  }

  (*  mk_queue : int -> t *)
  let mk_queue sz = 
    let h = make_heap (sz * 3 + 2) in
      {elems = h;
      head = (let hd = alloc h 1
              in assign_ptr h hd 0 (null h);
              hd);
      tail = (let tl = alloc h 1
              in assign_ptr h tl 0 (null h);
              tl);
    }
  
  (*  is_empty : t -> bool *)
  let is_empty (q:t) : bool = 
    let hd = deref_as_ptr q.elems q.head 0 and 
        tl =  deref_as_ptr q.elems q.tail 0 in 
    (is_null q.elems hd) && (is_null q.elems tl) 

  (*  is_full : t -> bool *)
  let is_full (q:t) : bool = false

  (*  enqueue -> t -> (int * string) -> unit *)
  (*  Add to tail *)
  let enqueue q (k,v) = 
    let n = mk_node q.elems k v in
    if is_empty q then () else 
    insert_after q.elems n (deref_as_ptr q.elems q.tail 0);
    assign_ptr q.elems q.tail 0 n

  (*  dequeue : t -> (int * string) option *)
  let dequeue (q: t) : (int * string) option = 
    let pop hd = 
      let nxt = next q.elems hd 
      and k = int_value q.elems hd
      and v = string_value q.elems hd in
      assign_ptr q.elems q.head 0 nxt;
      remove q.elems hd hd;
      Some (k, v)
    in
    if is_empty q then None else
    if is_null q.elems (deref_as_ptr q.elems q.head 0)
    (* Scenario 1: head is empty -> reverse tail and pop the first element of 
     * the reversed tail, set head to point to the first node of the reversed 
     * and set tail to point to null. *)
    then (let tl = deref_as_ptr q.elems q.tail 0 in
          let hd = reverse q.elems tl in    
          assign_ptr q.elems q.tail 0 (null q.elems);
          pop hd)
    (* Scenario 2: head isn't empty -> pop the node that head points to 
     * and set head to point to the next node. *)
    else (let hd = deref_as_ptr q.elems q.head 0 in
          pop hd)
         
  (*  queue_to_list : t -> (int * string) list *)
  (* walk returns in reverse order. e.g. If the SLL is
   * [(5, "a"); (9, "z")], walk will return a list 
   *  [(9, "z"); (5, "a")] *)
  (* Scenario 1: head is empty -> walk tail;
   * Scenario 2: head is not empty -> walk reversed head
   * and concat it with walk tail. *)
  let queue_to_list (q: t) : (int * string) list =
    let rec walk n a =
      if is_null q.elems n then a else
        walk (next q.elems n) ((int_value q.elems n, 
        string_value q.elems n) :: a)  

    in let hd = deref_as_ptr q.elems q.head 0 and 
          tl = deref_as_ptr q.elems q.tail 0 in
    if (is_null q.elems hd) 
    then walk tl []
    else walk (reverse q.elems hd) (walk tl []);
    
end

(******************************************************)
(*             Testing heap queue                     *)
(******************************************************)

open AllocatorImpl
module Q = HeapSLLQueue(AllocatorImpl)

open Q
open ArrayUtil

(********************************************) 
(* TODO: Add more tests on queue operations *)
(********************************************) 

let%test "first in first out" =
    let q = mk_queue 10 in 
    enqueue q (1, "a");
    enqueue q (2, "b");
    enqueue q (3, "c");
    let e = dequeue q in 
    e = Some (1, "a")


let%test "enque then dequeue the same elements gives empty queue" = 
  let q = mk_queue 10 in 
  enqueue q (1, "a");
  enqueue q (2, "b");
  enqueue q (3, "c");
  let _ = dequeue q in 
  let _ = dequeue q in 
  let _ = dequeue q in 
  is_empty q

let%test "queue to list" = 
  let q = mk_queue 10 in 
  enqueue q (1, "a");
  enqueue q (2, "b");
  enqueue q (3, "c");
  let l = queue_to_list q in 
  l = [(1, "a"); (2, "b"); (3, "c")]

let%test "dequeue gives none when all elements are out" = 
  let q = mk_queue 10 in 
  enqueue q (1, "a");
  enqueue q (2, "b");
  enqueue q (3, "c");
  let first = dequeue q in 
  let second = dequeue q in 
  let third = dequeue q in 
  let fourth = dequeue q in 
  first = Some (1, "a") &&
  second = Some (2, "b") &&
  third = Some (3 , "c") &&
  fourth = None

let%test "checking enqueue and dequeue many elements with queue_to_list" =
  let arr = generate_key_value_array 5 in
  let list = to_list arr in
  let q = mk_queue 5 in
  List.iter (fun kv -> enqueue q kv) list;
  list = queue_to_list q

(******************************************************)
(*         Testing heap reclamation                   *)
(******************************************************)



(*

Implement a test that creates a small heap, and then uses it to 
allocate and use a queue (by enqueueing and dequeueing), in a way 
that the number of nodes the queue has over its lifetime is *larger*
than the capacity of the heap. That is, make sure to use memory 
reclamation implemented for doubly-linked lists.

*)

let%test "heap reclamation 1" = 
    let q = mk_queue 5 in
    enqueue q (42, "a");
    enqueue q (15, "b");
    enqueue q (33, "c");
    enqueue q (52, "d");
    enqueue q (31, "e");
    let e1 = dequeue q in
    let e2 = dequeue q in
    let e3 = dequeue q in
    enqueue q (300, "new");
    enqueue q (235, "alloced");
    e1 = Some (42, "a") &&
    e2 = Some (15, "b") &&
    e3 = Some (33, "c")


let%test "heap reclamation simple" = 
  let q = mk_queue 5 in
  enqueue q (42, "a");
  let e1 = dequeue q in
  e1 = Some (42, "a")

let%test "heap reclamation 2" = 
  let q = mk_queue 5 in
  enqueue q (42, "a");
  enqueue q (15, "b");
  enqueue q (33, "c");
  enqueue q (52, "d");
  enqueue q (31, "e");
  let e1 = dequeue q in
  let e2 = dequeue q in
  let e3 = dequeue q in
  enqueue q (300, "new");
  enqueue q (235, "alloced");
  e1 = Some (42, "a") &&
  e2 = Some (15, "b") &&
  e3 = Some (33, "c")
    
(* Queue *)              
    
let%test "checking enqueue and dequeue many elements with queue_to_list" =
  let arr = generate_key_value_array 5 in
  let list = to_list arr in
  let q = mk_queue 5 in
  List.iter (fun kv -> enqueue q kv) list;
  list = queue_to_list q






