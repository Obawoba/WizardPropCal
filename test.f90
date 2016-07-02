module myType_mod
  type myType
    real::foo
    integer::bar
  end type myType
end module

program test
  use myType_mod
  implicit none

  interface operator (>)
      logical function compare(a,b)
        type(myType),intent(in) :: a,b
        compare = a%foo>b%foo
      end function compare
  end interface operator (>)

  type(myType) :: tfoo, tbar
  tfoo = card(1.,2); tbar = card(3.,4)
  print*, tfoo>tbar
end program test
