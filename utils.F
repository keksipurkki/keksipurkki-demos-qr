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

  interface sorted
    procedure sorted_r, sorted_c
  end interface

contains

  pure function sorted_r(X) result(Y)
    real(real64), intent(in) :: X(:)
    real(real64), target :: Y(size(X))
    integer(c_size_t) :: n, width
    Y = X
    n = size(Y, kind=c_size_t)
    width = c_sizeof(Y) / n
    call qsort(c_loc(Y), n, width, c_funloc(sort_real64))
  end function

  pure function sorted_c(X) result(Y)
    complex(real64), intent(in) :: X(:)
    complex(real64), target :: Y(size(X))
    integer(c_size_t) :: n, width
    Y = X
    n = size(Y, kind=c_size_t)
    width = c_sizeof(Y) / n
    call qsort(c_loc(Y), n, width, c_funloc(sort_complex64))
  end function

  function sort_real64(ap, bp) result(n) bind(C)
    type(c_ptr), value :: ap, bp
    real(real64), pointer :: a, b
    integer(c_int) :: n

    call c_f_pointer(ap, a)
    call c_f_pointer(bp, b)

    if (abs(a - b) < 10*epsilon(1.0_real64)) then
      n = 0
    else
      n = int(sign(1.0_real64, a - b))
    end if
  end

  function sort_complex64(ap, bp) result(n) bind(C)
    type(c_ptr), value :: ap, bp
    complex(real64), pointer :: a, b
    real(real64), target :: aa, bb
    integer(c_int) :: n

    call c_f_pointer(ap, a)
    call c_f_pointer(bp, b)

    aa = real(a)
    bb = real(b)

    n = sort_real64(c_loc(aa), c_loc(bb))

    if (n /= 0) then
      return
    endif

    aa = aimag(a)
    bb = aimag(b)

    n = sort_real64(c_loc(aa), c_loc(bb))

  end

  pure function eye(n)
    integer, intent(in) :: n
    integer :: i
    real(real64) :: eye(n, n)
    eye = 0.0d0
    do concurrent (i=1:n)
      eye(i, i) = 1.0d0
    enddo
  end function

  function diagonal_matrix(vec)
    real(real64), intent(in) :: vec(:)
    real(real64), allocatable, target :: diagonal_matrix(:,:)
    real(real64), pointer :: diag(:)
    integer :: n
    n = size(vec)
    allocate(diagonal_matrix(n, n), source=0.0d0)
    diag => diagonal(diagonal_matrix)
    diag = vec
  end function

  pure function zeros(n)
    integer, intent(in) :: n
    real(real64) :: zeros(n)
    zeros = 0.0d0
  end function

  function diagonal(H, offset) result(s)
    real(real64), intent(in), contiguous, target :: H(:, :)
    integer, value, optional :: offset
    real(real64), pointer :: tmp(:), s(:)
    integer :: n
    n = size(H, 1) + 1
    tmp(1:size(H)) => H
    if (.not. present(offset)) offset = 0
    if (offset < 0) offset = offset + n
    s => tmp(1 + offset :: n)
  end function

end module
