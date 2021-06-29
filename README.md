# Memory-Allocator

## Introduction

At the fundamental level, this project builds a memory allocator that allows for storing and retrieving integers, strings, and pointers in the memory without using OCaml reference. Based on this memory allocator, we define data structures such as Doubly-linked list (DLL), Singly-linked list (SLL), DLL-based queue, and SLL-based queue and implement their associated functions.

This is the midterm team project for YSC2229 Introductory Data Sctructures and Algorithms. It is dedicated to implementing and using data structures for dynamic memory allocation and reclamation. My contributions to the project are: alloc function and some allocator tests, SLL and tests, SLL-based queue tests.I uploaded my individual report "Report.pdf" where I explain data structures we used in the project and other design choices.

## Memory Allocator from Scratch

In this project, the data type of the memory is called heap. We choose to use **array** as the heap because we can retrieve any element in constant time given its index. We initially build three arrays for storing pointers, integers, and strings respectively, but after learning OCaml’s algebraic data type, we decide to have only one array whose elements belong to a variant type. This way, we maximize the use of memory spaces in the heap.

To create an array where different types can be stored, we define a variant type for the array’s elements. The type has five constructors: Free, Init, P of ptr, I of int, and S of string. First, when the heap is just created, all elements are filled with Free, indicating that the cells in the heap are available for storing data. The memory cells can also take the form of an algebraic data type P of ptr, I of int, or S of string, signifying that the cell is currently being used to store the corresponding data type, which are pointer, integer, or string.

The pointer is a data type that specifies a location in the memory. Since the memory takes the form of an array, the pointer either carries the index of an array element or nothing. Therefore, we give it the type int option. With pointers, data in the memory can be linked as they can point to other data.


... For more descriptions and illustrations please see Report.pdf

