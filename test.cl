(* hairy  . . .*)
-- IO -> Bazz -> Foo -> Razz -> Bar

class Class3 inherits Class2 {
     b : Int <- g.doh() + printh();
};

class Class2 inherits IO {
     h : Int <- 1;
     g : Class3  <- case self of
                        n : Class2 => new Class3;
                        n : Class3 => n;
		            esac;

     printh() : Int { { out_int(h); 0; } };

     doh() : Int { h <- h + 1 };
};

(* scary . . . *)
class Main {
  a : Class2 <- new Class2;
  main(): String { "do nothing" };
};





