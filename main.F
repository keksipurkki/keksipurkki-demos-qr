program main
  use display
  use utils
  use eigenvalues
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: X(:, :)
  real(real64), allocatable :: Q(:, :), L(:)
  integer :: n = 10

  call read_input('input.nml', n)

  X = data_matrix(n)
  call disp('shape(X)', shape(X), orient='row', style='pad')

  call disp()
  call eig(X, Q, L)
  L = sorted(L)

  call disp('Λ', L, digmax=15, style='pad')
  call disp()

  call output_eigenvalues('lambda.txt', L, 15)

contains

  function data_matrix(n)
    integer, intent(in) :: n
    real(real64) :: data_matrix(n, n)
    integer :: i

    data_matrix = -2.0d0

    do i = 1, size(data_matrix, 1)
      data_matrix(i, i) = i
    end do

  end function

  subroutine read_input(fname, n)
    character(len=*), intent(in) :: fname
    integer, intent(out) :: n
    integer :: io, rc
    namelist /config/ n

    inquire (file=fname, iostat=rc)

    if (rc /= 0) then
      write (error_unit, '("Error: input file ", a, " does not exist")') fname
      return
    end if

    open (action='read', file=fname, iostat=rc, newunit=io)
    read (nml=config, iostat=rc, unit=io)
    if (rc /= 0) write (error_unit, '("Error: invalid Namelist format")')
    close (io)
  end subroutine

  subroutine output_eigenvalues(fname, L, digmax)
    character(len=*), intent(in) :: fname
    real(real64), intent(in) :: L(:)
    integer, intent(in) :: digmax
    integer :: io

    open (newunit=io, file=fname, status='replace', action='write', encoding='utf-8')
    call disp(L, unit=io, digmax=digmax)
    close (io)

  end subroutine

end program
