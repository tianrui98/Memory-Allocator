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
open DoublyLinkedList

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


(******************************************************)
(*        A queue based on doubly linked lists        *)
(******************************************************)

module HeapDLLQueue (A: Allocator) : Queue = struct
  module DLL = DoublyLinkedList(A)
  open A
  open DLL
         
  (* Opaque queue type *)
  type t = {
    elems: heap;
    head : ptr;
    tail : ptr
  }
  
  let mk_queue sz =
    let h = make_heap (sz * 4 + 2) in {
      elems = h;
      head = (let hd = alloc h 1
              in assign_ptr h hd 0 (null h);
              hd);
      tail = (let tl = alloc h 1
              in assign_ptr h tl 0 (null h);
              tl)
    }
  
  let is_empty q = deref_as_ptr q.elems q.head 0 = null q.elems
  
  let is_full q = false
  
  let enqueue q (k,v) =
    let n = mk_node q.elems k v in
    (if is_empty q
     then assign_ptr q.elems q.head 0 n);
    (let tl = deref_as_ptr q.elems q.tail 0 in
     if tl = null q.elems
     then ()
     else insert_after q.elems tl n);
    assign_ptr q.elems q.tail 0 n
  
  let dequeue q =
    if is_empty q
    then None
    else (let hd = deref_as_ptr q.elems q.head 0 in
          let nxt = next q.elems hd in
          let k = int_value q.elems hd in
          let v = string_value q.elems hd in
          assign_ptr q.elems q.head 0 nxt;
          (if hd = deref_as_ptr q.elems q.tail 0
           then assign_ptr q.elems q.tail 0 (null q.elems));
          remove q.elems hd;
          Some (k, v))
  
  let queue_to_list q =
    let rec dequeue_into_list q =
      if is_empty q
      then []
      else (let e = dequeue q |> get_exn in
            e :: dequeue_into_list q)
    in dequeue_into_list q
        
end

(******************************************************)
(*             Testing heap queue                     *)
(******************************************************)

open AllocatorImpl
module Q = HeapDLLQueue(AllocatorImpl)

open Q
open ArrayUtil

let%test "basic queue operations" = 
  let q = mk_queue 10 in
  enqueue q (42, "a");
  let e = dequeue q in
  e = Some (42, "a")

let%test "queue is empty" =
  let q = mk_queue 10 in
  enqueue q (42, "a");
  let _ = dequeue q in ();
  is_empty q

let%test "basic queue operations alt" = 
  let q = mk_queue 10 in
  enqueue q (42, "a");
  let _ = dequeue q in ();
  enqueue q (42, "a");
  let e = dequeue q in
  e = Some (42, "a")

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

let%test "heap reclamation: enqueue N > 2 items on size 2 queue" =
  let q = mk_queue 2 in
  enqueue q (42, "a");
  enqueue q (42, "a");
  let _ = dequeue q in ();
  enqueue q (42, "a");
  true

let%test "dequeue from empty queue" =
  let q = mk_queue 2 in
  enqueue q (42, "a");
  let _ = dequeue q in ();
  let res = dequeue q in
  res = None
