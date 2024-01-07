include 'assertions.F'

program main
  use display
  use utils
  use eigenvalues
  use assertions
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: X(:, :)
  real(real64), allocatable :: Q(:, :), L(:), LL(:)
  integer :: n = 10

  call read_input('input.nml', n)

  X = data_matrix(n)
  call disp('shape(X)', shape(X), orient='row', style='pad')

  call disp()
  call eig(X, Q, L)
  L = sorted(L)

  call disp('Λ', L, digmax=15, style='pad', advance='no')
  LL = sorted(reference_values(data_matrix(n), n))
  call disp('Λref', LL, digmax=15, style='pad', advance='no')
  call disp('Abs. error', abs(L - LL), style='pad', fmt='ES15.3')

  call output('lambda.txt', L, 15)
  call output('data.txt', reshape(X, [size(X)]), 15)

  call assert(maxval(abs(L - LL)) < 1e-6, 'Maximum absolute error threshold exceeded')

contains

  function data_matrix(n)
    integer, intent(in) :: n
    real(real64) :: data_matrix(n, n)
    integer :: i

    data_matrix = -2.0d0

    do i = 1, size(data_matrix, 1)
      data_matrix(i, i) = i
    end do

    !data_matrix(2,1) = 1.0d0

  end function

  function reference_values(X, n) result(L)
    real(real64), intent(in) :: X(:,:)
    integer, intent(in) :: n
    real(real64) :: L(n), Limag(n)
    real(real64), pointer :: Z(:)
    integer :: info
    L = 0.0d0
    Limag = 0.0d0
    Z => null()
    call dlahqr(.false., .false., n, 1, n, eig_hessenberg(X), n, L, Limag, 1, n, Z, 1, info)
    call assert(info == 0, 'dlahqr failed')
    call assert(all(Limag == 0), 'complex-valued eigenvalues detected')
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

  subroutine output(fname, V, digmax)
    character(len=*), intent(in) :: fname
    real(real64), intent(in) :: V(:)
    integer, intent(in) :: digmax
    integer :: io

    open (newunit=io, file=fname, status='replace', action='write', encoding='utf-8')
    call disp(V, unit=io, digmax=digmax)
    close (io)

  end subroutine

end program