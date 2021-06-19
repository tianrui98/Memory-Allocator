(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2021 Ilya Sergey

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
(* A singly linked list parameterised by an allocator *)
(*           Only for teams of size 3                 *)
(******************************************************)

module SinglyLinkedList (A: Allocator) = struct
    open A 

    type sll_node = ptr 

    let mk_node heap i s : sll_node = 
      let p = (alloc heap 3) in 
      (assign_int heap p 0 i ;
      assign_string heap p 1 s; 
      assign_ptr heap p 2 (null heap);
      p)

    (*  In SLL, prev does not exist *)
    (*  next : A.heap -> sll_node -> sll_node *)
    let next heap (n : sll_node) : sll_node = deref_as_ptr heap n 2

    (*  int_value : A.heap -> sll_node -> int *)
    let int_value heap (n : sll_node) : int = deref_as_int heap n 0

    (*  string_value : A.heap -> sll_node -> string *)
    let string_value heap (n : sll_node) : string = deref_as_string heap n 1

    (*  insert_after : A.heap -> sll_node -> sll_node -> unit *)
    
    let insert_after heap (n1 : sll_node) (n2 : sll_node) : unit = 
      (* change n1's pointer to point at n2*)
      let n3 = next heap n1 in 
      (if (is_null heap n3)
      then ()
      (*if there is a node after n1, change n2's next pointer to point at n3*)
      else assign_ptr heap n2 2 n3 );
      (*regardless of n3*)
      (*change n1's next ptr to point at n2 *)
      assign_ptr heap n1 2 n2

    (*  reverse : A.heap -> sll_node -> sll_node *)
    (*if there are 3 linked nodes n1->n2->n3 *)
    (*change n3's next to n2 *)
    (*change n2's next to n1 *)
    (*change n1's next to null *)
    (*return the new headnode n3*)
    (* if head node points to null, do nothing.return head node*)
    let reverse heap (hd_node: sll_node) : sll_node =
      let null_ptr = null heap in 
      if is_null heap (deref_as_ptr heap hd_node 2) then hd_node
      else
        let rec rev cur prev  =
          let p = deref_as_ptr heap cur 2 in
          assign_ptr heap cur 2 prev ; 
          if p = null_ptr then cur
          else rev p cur
          in rev hd_node (null heap)


    (*  remove : A.heap -> sll_node -> sll_node -> unit *)
    (* suppose there are 3 nodes n1->n2->n3 *)
    (* remove n2 means: *)
    (* - Free up the memory where n2's info is stored *)
    (* - Restore the connection  n1 -> n3 *)
    (* THEREFORE, we need to find n1 and to n3 *)
    (* HOWEVER, if we only want to remove the head node,
    we only need to free up the memory*)
    let remove heap n hd_node : unit = 
      if n = hd_node then ()
      else
        (let next_node = deref_as_ptr heap n 2 in 
        let rec find_prev cur = 
          let p = deref_as_ptr heap cur 2 in 
          if p = n then cur
          else find_prev p 
        in
          let prev_node = find_prev hd_node in 
          assign_ptr heap prev_node 2 next_node );
        free heap n 3

    (*  print_from_node : A.heap -> sll_node -> unit *)

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

(**********************************************)
(*              Testing SLL                   *)
(**********************************************)

open AllocatorImpl

(* Concrete allocator *)
module SLLImpl = SinglyLinkedList(AllocatorImpl) 

open SLLImpl 

let%test "basic node manipulation" =
   let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b" in
    insert_after heap n1 n2;
    let n = next heap n1 in
    let i = int_value heap n in
    let s = string_value heap n in
    i = 2 && s = "b"
    
(*************************)
(* TODO: Add more tests. *)
(*************************)

(*test remove *)
let%test "remove the last node" = 
  let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    remove heap n3 n1; 
    (*n2's next should be null*)
    let n = next heap n2 in 
    AllocatorImpl.is_null heap n

let%test "remove the middle node" = 
  let heap = AllocatorImpl.make_heap 13 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c"
    and n4 = mk_node heap 4 "d" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    insert_after heap n3 n4;
    remove heap n3 n1 ;
    (*n2's next should be n4*)
    let n = next heap n2  in 
    let i = int_value heap n in
    let s = string_value heap n in
    i = 4 && s = "d"  
 
let%test "remove the head node" = 
  let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    remove heap n1 n1;
    (*n2's next should be n3. no change*)
    let n = next heap n2 in
    let i = int_value heap n in
    let s = string_value heap n in
    i = 3 && s = "c"

let%test "remove everything except the first node" = 
  let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    remove heap n2 n1;
    remove heap n3 n1; 
    (*n1's next should be nul*)
    let n = next heap n1 in
    AllocatorImpl.is_null heap n

(*remove all elements then add the elements back.*)
(*shoudld not throw memory error*)
let%test "remove everything then add back in" = 
  let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    remove heap n2 n1;
    remove heap n3 n1;
    remove heap n1 n1;
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    true

(*test reverse*)
(*reverse n1->n2->n3,head node shoudld be n3*)
let%test "reverse check new head node" = 
  let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    reverse heap n1 = n3

(*reverse twice should give the same list *)
let%test "reverse twice" = 
  let heap = AllocatorImpl.make_heap 10 in
    let n1 = mk_node heap 1 "a"
    and n2 = mk_node heap 2 "b"
    and n3 = mk_node heap 3 "c" in 
    insert_after heap n1 n2;
    insert_after heap n2 n3;
    let _ = reverse heap n1 in 
    let _ = reverse heap n3 in
    let next_of_1 = next heap n1 in
    let i1 = int_value heap next_of_1 in
    let s1 = string_value heap next_of_1 in
    let next_of_2 = next heap n2 in 
    let i2 = int_value heap next_of_2 in 
    let s2 = string_value heap next_of_2 in 
    i1 = 2 && s1 = "b" &&
    i2 = 3 && s2 = "c"

let%test "reverse when head node points to null" = 
  let heap = AllocatorImpl.make_heap 10 in 
  let n1 = mk_node heap 1 "a"
  and n2 = mk_node heap 2 "b"
  and n3 = mk_node heap 3 "c" in 
  insert_after heap n2 n3;
  let res = reverse heap n1 in 
  let next_of_2 = next heap n2 in 
  let i2 = int_value heap next_of_2 
  and s2 = string_value heap next_of_2 in 
  (*return the head node*)
  res = n1 &&
  (*order of n2 n3 does not change *)
  i2 = 3 &&
  s2 = "c"

  
let%test "make too many nodes" = 
  let heap = AllocatorImpl.make_heap 9 in 
  let _ = mk_node heap 1 "a"
  and _ = mk_node heap 2 "b"
  and _ = mk_node heap 3 "c" in 
  try let _ = mk_node heap 5 "d" in false
  with Failure _ -> true

(*A test for print_from_node *)

(* let heap = AllocatorImpl.make_heap 20 in
  let n1 = mk_node heap 1 "a" 
  and n2 = mk_node heap 2 "b"
  and n3 = mk_node heap 3 "c" in
  insert_after heap n1 n2;
  insert_after heap n2 n3;
  print_from_node heap n1;
  let _ = reverse heap n1 in 
  print_from_node heap n3;;  *)
