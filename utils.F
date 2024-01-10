module utils
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding
  implicit none

  interface
    ! void qsort(void *base, size_t nel, size_t width, int (*compar)(const void *, const void *));
    pure subroutine qsort(base, nel, width, compar) bind(C)
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: base
      integer(c_size_t), value :: nel, width
      type(c_funptr), value :: compar
    end subroutine
  end interface

contains

  pure function sorted(X) result(Y)
    real(real64), intent(in) :: X(:)
    real(real64), target :: Y(size(X))
    integer(c_size_t) :: n, width
    Y = X
    n = size(Y, kind=c_size_t)
    width = c_sizeof(Y) / n
    call qsort(c_loc(Y), n, width, c_funloc(sort_real64))
  end function

  function sort_real64(ap, bp) result(n)
    type(c_ptr), value :: ap, bp
    real(real64), pointer :: a, b
    integer(c_int) :: n

    call c_f_pointer(ap, a)
    call c_f_pointer(bp, b)

    if (a == b) then
      n = 0
    else
      n = int(sign(1.0_real64, a - b))
    end if
  end function

  pure function eye(n)
    integer, intent(in) :: n
    integer :: i
    real(real64) :: eye(n, n)
    eye = 0.0d0
    forall (i=1:n) eye(i, i) = 1.0d0
  end function

  pure function zeros(n)
    integer, intent(in) :: n
    real(real64) :: zeros(n)
    zeros = 0.0d0
  end function

end module
