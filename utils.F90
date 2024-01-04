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

    function subdiagonal(H) result(s)
      real(real64), intent(in), contiguous, target :: H(:,:)
      real(real64), pointer :: tmp(:), s(:)
      tmp(1:size(H)) => H
      s => tmp(2::size(H,1) + 1)
    end function

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
      endif
    end function

end module
