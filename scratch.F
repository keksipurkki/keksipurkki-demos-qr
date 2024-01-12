module m
  use iso_fortran_env
  implicit none

  interface polymorphic_return
    module procedure return_default, return_real, return_complex
  end interface

contains

  !function polymorphic_return(mold) result(poly)
  !  class(*), intent(in) :: mold
  !  class(*), allocatable :: poly(:)
  !  select type(mold)
  !  type is (real(real64))
  !    poly = return_real()
  !  type is (complex(real64))
  !    poly = return_complex()
  !  class default
  !    error stop
  !  end select
  !end

  function return_default() result(y)
    real(real64) :: y(6)
    y = return_real(1.0d0)
  end

  function return_real(mold) result(y)
    real(real64) :: mold(..)
    real(real64) :: y(6)
    if(rank(mold)/=rank(mold)) continue
    y = 1.0d0
  end

  function return_complex(mold) result(z)
    complex(real64) :: mold(..)
    complex(real64) :: z(6)
    if(rank(mold)/=rank(mold)) continue
    z = (1.0d0, 0.0d0)
  end

end module

program test
  use iso_fortran_env
  use display
  use utils
  use m
  implicit none
  real(real64) :: one = 1.0d0
  real(real64), allocatable :: real_lambdas(:)
  complex(real64), allocatable :: complex_lambdas(:)

  real_lambdas = polymorphic_return(mold=real_lambdas)
  complex_lambdas = polymorphic_return(mold=complex_lambdas)

  call disp(sorted(real_lambdas))
  call disp(sorted(complex_lambdas))
  call disp(sorted(polymorphic_return(mold=one)))
  call disp(sorted(polymorphic_return()))

  contains

end program
