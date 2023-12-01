
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  d: Object <- new Int;
  main():Int { {isvoid(d); 0; 1; 2;} };
};

(* what exactly is heap memory? Well it is whenever somebody calls object copy, 
that is the only time that we allocate anything on the heap*)
