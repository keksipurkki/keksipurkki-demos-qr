module polymorphic
  use iso_fortran_env
  implicit none

  type foo
  end type

  interface assignment(=)
    module procedure assign_one
  end interface

contains
  pure integer function integer_one()
    integer_one = 1
  end

  pure real function real_one()
    real_one = 1.0
  end

  pure subroutine assign_one(lhs, rhs)
    class(*), intent(in out) :: lhs
    type(foo), intent(in) :: rhs
    select type(lhs)  
      type is (integer)
        lhs = integer_one()
      type is (real(real64)) 
        lhs = real_one()
    end select
  end subroutine


end module

program test
  use iso_fortran_env
  use display
  use polymorphic
  implicit none

  integer :: i
  real(real64) :: x

  !call assign_one(i, foo())
  !call assign_one(x, foo())

  i = foo()
  x = foo()

  call disp(i)
  call disp(x)

end program
