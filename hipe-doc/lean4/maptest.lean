
def mymap (xs : List UInt64) :=
  match xs with
  | [] => []
  | x :: xs => (x + 100) :: mymap xs