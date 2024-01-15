#include "assertions.F"

module parameters
  use iso_fortran_env
  implicit none
  real(real64), parameter :: max_relative_error = 1e-10
  logical :: enable_trace = .false.
  integer :: ndim = 10
  integer :: ntimes = 10
  contains

  subroutine read_parameters(fname)
    character(len=*), intent(in) :: fname
    integer :: io, rc
    namelist /config/ ndim, ntimes, enable_trace

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

end module

program main
  use parameters
  use display
  use utils
  use eigenvalues
  use assertions
  use benchmarks, only: benchmark_data
  use benchmarks, only: pseudo_random_lower_hessenberg
  use iso_fortran_env
  implicit none

  integer ::  k

#ifdef _DEBUG
  call disp_set(digmax=16)
#else
  call disp_set(digmax=5)
#endif

  call read_parameters('input.nml')
  call disp('ndim = ', ndim)
  call disp('ntimes = ', ntimes)

  do k = 1, ntimes
    call benchmark(ndim, pseudo_random_lower_hessenberg)
  enddo

  ! ... Frank matrix

  ! ... trivial_matrices

  ! ... positive definite matrices

  ! ... non_hermitian_eigenvalue_problems (hand picked)

  ! ... ieee_754_corner_cases (lambdas are widely different, lambdas are very near each other, one lambda is Infinity?)

  ! ... permutation_matrices, |lambda_i| = 1, i...ndim

  ! ... antisymmetrical matrices (purely imaginary eigenvalues)

contains

  subroutine benchmark(n, src)
    integer, intent(in) :: n
    procedure(benchmark_data) :: src
    real(real64), allocatable :: X(:, :)

    complex(real64), allocatable :: L(:), LL(:)
    real(real64), allocatable :: err(:, :)
    character(:), allocatable :: fmt

#ifdef _DEBUG
  fmt = 'ES16.10'
#else
  fmt = 'ES15.3'
#endif

    allocate (err(2, n), source=0.0d0)

    X = src(n)

    call disp(repeat('*', 121))
    call disp()
#ifdef _DEBUG
    call disp('X = ', X)
#endif

    call eig(X, L, callback=trace_callback)
    call eig_lapack(X, LL, n)

    L = sorted(L)
    LL = sorted(LL)

    if (all(imag([L,LL]) == 0.d0)) then

      err(1, :) = abs(1.0d0 - real(L) / real(LL))
      err(2, :) = abs(real(L) - real(LL))

      call disp()
      call disp('Maximum relative error = ', maxval(err(1, :)))
      call disp('Maximum absolute error = ', maxval(err(2, :)))
      call disp()

      call disp('Λ', real(L), style='pad', advance='no')
      call disp('Λref', real(LL), style='pad', advance='no')

    else

      err(1, :) = abs(1.0d0 - L / LL)
      err(2, :) = abs(L - LL)

      call disp()
      call disp('Maximum relative error = ', maxval(err(1, :)))
      call disp('Maximum absolute error = ', maxval(err(2, :)))
      call disp()

      call disp('Λ', L, style='pad', advance='no')
      call disp('Λref', LL, style='pad', advance='no')

    endif

    call disp('Rel. error', err(1, :), fmt=fmt, style='pad', advance='no')
    call disp('Abs. error', err(2, :), fmt=fmt, style='pad')
    call assert(maxval(err(1, :)) < max_relative_error, 'Maximum error threshold exceeded')

  end

  subroutine eig_lapack(X, L, n)
    real(real64), intent(in) :: X(:, :)
    complex(real64), allocatable, intent(out) :: L(:)
    integer, intent(in) :: n
    real(real64) :: Lreal(n), Limag(n)
    real(real64) :: Z(1, 1), H(n, n)
    integer :: info

    Lreal = 0.0d0
    Limag = 0.0d0
    H = eig_hessenberg_matrix(X)
    call dlahqr(.false., .false., n, 1, n, H, n, Lreal, Limag, 1, n, Z, 1, info)
    call assert(info == 0, 'dlahqr failed')
    L = cmplx(Lreal, Limag, kind=real64)
  end

  subroutine output(fname, V, digmax)
    character(len=*), intent(in) :: fname
    real(real64), intent(in) :: V(:)
    integer, intent(in) :: digmax
    integer :: io

    open (newunit=io, file=fname, status='replace', action='write', encoding='utf-8')
    call disp(V, unit=io, digmax=digmax)
    close (io)
  end subroutine

  subroutine trace_callback(err, H)
    integer, intent(in) :: err
    real(real64), intent(in) :: H(:,:)
    real(real64), save :: tic = -1d0, toc = -1d0

    if (err /= 0) error stop err

    if (enable_trace) then
      call disp()
      toc = tic
      call cpu_time(tic)
      if (toc >= 0) then
        call disp('perf:', [1.0d0*shape(H), tic - toc], orient='row')
      endif
      call disp('H', H, style='pad', digmax=16)
      call disp()
    endif
  end subroutine

end program
