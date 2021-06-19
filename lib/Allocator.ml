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


(****************************************************)
(*           Allocator signature                    *)
(****************************************************)

module type Allocator = sig

  (* An abstract type for dynamic storage - clients don't have to know what it is  *)
  type heap
  (* An abstract type for the pointer (address in the dynamic storage)             *)
  type ptr

  (* Create a new heap of given size.                                              *)
  val make_heap : int -> heap

  (* Returns the "null" pointer. Noting can be assigned to it (results in error)   *)
  val null : heap -> ptr
  (* Tests whether the pointer is null                                             *)  
  val is_null : heap -> ptr -> bool

  (***                       Operations with pointers                            ***)
  (* All should throw exceptions for if the pointer is_null                        *)  

  (* Allocating a contiguous segment of dynamically-typed pointers in a heap.      *)
  (* Throws an specific "Out-Of-Memory" error if no allocation is possible.        *)
  val alloc : heap -> int -> ptr
  (* Frees the space in heap taken by the pointer.                                 *)
  val free : heap -> ptr -> int -> unit

  (* Dereferencing a pointer with an offset [0..n] obtaining a value it points to  *)

  (* Dereference as an pointer, throw an exception if the target is not an pointer *)  
  val deref_as_ptr : heap -> ptr -> int -> ptr
  (* Dereference as an integer, throw an exception if the target is not an integer *)  
  val deref_as_int : heap -> ptr -> int -> int
  (* Dereference as an integer, throw an exception if the target is not an string  *)  
  val deref_as_string : heap -> ptr -> int -> string

  (* Assigning values to a pointer with an offset.                                 *)
  (* Should throw an "Out-Of-Memory" error if not possible to create a new value   *)
  (* The last argument is a value being assigned (of the corresponding type)       *)
  val assign_ptr : heap -> ptr -> int -> ptr -> unit
  val assign_int : heap -> ptr -> int -> int -> unit
  val assign_string : heap -> ptr -> int -> string -> unit

end
