#include "assertions.F"

module parameters
  use iso_fortran_env
  implicit none
  logical :: enable_trace = .false.
  integer :: ndim = 10
  integer :: ntimes = 10

#ifdef _DEBUG
  character(*), parameter :: fmt = 'ES16.10'
#else
  character(*), parameter :: fmt = 'ES15.3'
#endif

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
  use benchmarks, only: rotation_matrix
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
    call benchmark(ndim)
  enddo

  ! ... Frank matrix

  ! ... trivial_matrices

  ! ... positive definite matrices

  ! ... non_hermitian_eigenvalue_problems (hand picked)

  ! ... ieee_754_corner_cases (lambdas are widely different, lambdas are very near each other, one lambda is Infinity?)

  ! ... permutation_matrices, |lambda_i| = 1, i...ndim

  ! ... antisymmetrical matrices (purely imaginary eigenvalues)

contains

  subroutine benchmark(n)
    integer, intent(in) :: n
    real(real64), allocatable :: M(:,:), X(:, :)
    real(real64), allocatable :: LL(:)
    real(real64) :: max_relative_error

    complex(real64), allocatable :: L(:)
    real(real64), allocatable :: err(:, :)
    integer :: k

    max_relative_error = 1d-13 ! TODO: Work on this
    allocate (err(n, 2), source=0.0d0)

    LL = [( 1d0*k, k = 1, n )]
    M = rotation_matrix(n)

    X = matmul(M, matmul(diagonal_matrix(LL), transpose(M)))

    call disp(repeat('*', 121))
    call disp()

#ifdef _DEBUG
    call output('data.txt', X, digmax=15)
#endif

    call eig(X, L, callback=trace_callback)
    L = sorted(L)

    err(:, 1) = abs(1.0d0 - (L / LL))
    err(:, 2) = abs(L - LL)

    call disp()
    call disp('Error tolerance = ', max_relative_error)
    call disp('Maximum relative error = ', maxval(err(:, 1)))
    call disp('Maximum absolute error = ', maxval(err(:, 2)))
    call disp()

    if (all(L == conjg(L))) then
      call disp('Λ', real(L), style='pad', advance='no')
      call disp('Λref', real(LL), style='pad', advance='no')
    else
      call disp('Λ', L, style='pad', advance='no')
      call disp('Λref', LL, style='pad', advance='no')
    endif

    call disp('Rel. error', err(:, 1), fmt=fmt, style='pad', advance='no')
    call disp('Abs. error', err(:, 2), fmt=fmt, style='pad')
    call assert(maxval(err(:, 1)) < max_relative_error, 'Maximum error threshold exceeded')

  end

  subroutine output(fname, V, digmax)
    character(len=*), intent(in) :: fname
    real(real64), intent(in) :: V(..)
    integer, intent(in) :: digmax
    integer :: io

    select rank(V)
      rank(1)
      rank(2)
        open (newunit=io, file=fname, status='replace', action='write', encoding='utf-8')
        call disp(V, unit=io, digmax=digmax)
        close (io)
      rank default
        error stop 'Output is only supported for rank-1 and rank-2 arrays'
    end select

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
        call disp('perf: ', shape(H), orient='row', advance='no')
        call disp(' ', tic - toc, fmt=fmt)
      endif
      call disp('H', H, style='pad', digmax=16)
      call disp()
    endif
  end subroutine

end program
