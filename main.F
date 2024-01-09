include 'assertions.F'

program main
  use display
  use utils
  use eigenvalues
  use assertions
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: X(:, :), H(:, :)
  real(real64), allocatable :: L(:), LL(:), err(:,:)
  integer :: n = 10

  call read_input('input.nml', n)

  allocate(err(2,n), source=0.0d0)

  X = data_matrix(n)
  call disp('shape(X)', shape(X), orient='row', style='pad')

  H = eig_hessenberg(X)
  call disp('H = ', H, fmt='ES15.5')

  call disp()
  L = sorted(real(eig(H), kind=real64))
  LL = sorted(real(reference_values(data_matrix(n), n), kind=real64))

  err(1,:) = abs(1.0d0 - L/LL)
  err(2,:) = abs(L - LL)
  call disp('Maximum relative error = ', maxval(err(1,:)), digmax=16)
  call disp('Maximum absolute error = ', maxval(err(2,:)), digmax=16)
  call disp()

  call disp('Λ', L, digmax=15, style='pad', advance='no')
  call disp('Λref', LL, digmax=15, style='pad', advance='no')
  call disp('Rel. error', err(1,:), style='pad', fmt='ES15.3', advance='no')
  call disp('Abs. error', err(2,:), style='pad', fmt='ES15.3')

  call output('lambda.txt', L, 15)
  call output('data.txt', reshape(X, [size(X)]), 15)

  call assert(maxval(err(1,:)) < 1e-6, 'Maximum error threshold exceeded')

contains

  function data_matrix(n)
    integer, intent(in) :: n
    real(real64) :: data_matrix(n, n)
    integer :: i

    data_matrix = -2.0d0

    do i = 1, size(data_matrix, 1)
      data_matrix(i, i) = i
    end do

    data_matrix(2,1) = 1.0d0

  end function

  function reference_values(X, n) result(L)
    real(real64), intent(in) :: X(:,:)
    integer, intent(in) :: n
    real(real64) :: Lreal(n), Limag(n)
    real(real64) :: L(n)
    real(real64), pointer :: Z(:)
    integer :: info

    Lreal = 0.0d0
    Limag = 0.0d0
    Z => null()
    call dlahqr(.false., .false., n, 1, n, eig_hessenberg(X), n, Lreal, Limag, 1, n, Z, 1, info)
    call assert(info == 0, 'dlahqr failed')
    call assert(all(Limag == 0), 'complex-valued eigenvalues detected')
    L = Lreal
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
