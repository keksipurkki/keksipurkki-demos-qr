program main
  use display
  use utils
  use eigenvalues
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: X(:,:)
  real(real64), allocatable :: Q(:,:), L(:)

  X = data_matrix(100)

  call disp()
  call eig(X, Q, L)
  L = sorted(L)

  call disp()
  call disp('Λ =', L, digmax=15)
  call disp()

  call output_eigenvalues('lambda.txt', L, 15)

  contains

    function data_matrix(n)
      integer, intent(in) :: n
      real(real64) :: data_matrix(n, n)
      integer :: i

      data_matrix = -2.0d0

      do i = i, size(data_matrix, 1)
        data_matrix(i,i) = i
      enddo

    end function

    subroutine output_eigenvalues(fname, L, digmax)
      character(len=*), intent(in) :: fname
      real(real64), intent(in) :: L(:)
      integer, intent(in) :: digmax
      integer :: io

      open(newunit=io, file=fname, status="replace", action="write", encoding='utf-8')
      call disp(L, unit=io, digmax=digmax)
      close(io)

    end subroutine

end program
