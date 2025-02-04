(* hairy  . . .*)
-- IO -> Bazz -> Foo -> Razz -> Bar

class Class3 inherits Class2 {
     b : Int <- g.doh() + printh();
     getName(): String { "Class3" };
};

class Class2 inherits IO {
     h : Int <- 1;
     getName(): String { "Class2" };

     g : Class3  <- case self of
                        n : Class2 => { out_string("Making a Class3\n"); (new Class3); };
                        n : Class3 => { out_string("Returning a Class3\n"); n; };
		            esac;

     i : Object <- printh();

     printh() : Int { { out_string("Printing from: "); out_string(getName()); out_string(": "); out_int(h); out_string("\n"); 0; } };

     doh() : Int { h <- h + 1 };
};

(* scary . . . *)
class Main {
  a : Class2 <- new Class2;
  main(): String { "do nothing" };
};





