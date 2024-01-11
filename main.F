#include "assertions.F"

program main
  use display
  use utils
  use eigenvalues
  use assertions
  use benchmarks, only :: pseudo_random_lower_hessenberg
  use iso_fortran_env
  implicit none

  integer :: ndim = 10, ntimes = 10, k

#ifdef _DEBUG
  call disp_set(digmax=16)
#else
  call disp_set(digmax=5)
#endif

  call read_input('input.nml', ndim, ntimes)
  call disp('ndim = ', ndim)
  call disp('ntimes = ', ntimes)

  do k = 1, ntimes
    call benchmark(ndim, pseudo_random_lower_hessenberg)
  enddo

  ! ... trivial_matrices

  ! ... positive definite matrices

  ! ... non_hermitian_eigenvalue_problems (hand picked)

  ! ... ieee_754_corner_cases (lambdas are widely different, lambdas are very near each other, one lambda is Infinity?)

  ! ... permutation_matrices, |lambda_i| = 1, i...ndim

  ! ... antisymmetrical matrices (purely imaginary eigenvalues)

contains

  abstract interface
    function matrix_supplier(n)
      use iso_fortran_env
      integer, intent(in) :: n 
      real(real64), allocatable :: matrix_supplier(:,:)
    end function
  end interface

  subroutine benchmark(n, src)
    integer, intent(in) :: n
    type(matrix_supplier), intent(in) :: src

    real(real64), allocatable :: X(:, :)
    complex(real64), allocatable :: L(:), LL(:)
    real(real64), allocatable :: err(:, :)
    integer :: info
    character(:), allocatable :: fmt

#ifdef _DEBUG
  fmt = 'ES16.10'
#else
  fmt = 'ES15.3'
#endif

    allocate (err(2, n), source=0.0d0)

    X = src(n)

    call disp('X = ', X)

    !L = sorted(eig(X, info=info))
    !LL = sorted(eig_ref(X, n))

    !if (all(imag([L,LL]) == 0.d0)) then

    !  err(1, :) = abs(1.0d0 - real(L) / real(LL))
    !  err(2, :) = abs(real(L) - real(LL))

    !  call disp()
    !  call disp('Maximum relative error = ', maxval(err(1, :)))
    !  call disp('Maximum absolute error = ', maxval(err(2, :)))
    !  call disp()

    !  call disp('Λ', real(L), style='pad', advance='no')
    !  call disp('Λref', real(LL), style='pad', advance='no')

    !else

    !  err(1, :) = abs(1.0d0 - L / LL)
    !  err(2, :) = abs(L - LL)

    !  call disp()
    !  call disp('Maximum relative error = ', maxval(err(1, :)))
    !  call disp('Maximum absolute error = ', maxval(err(2, :)))
    !  call disp()

    !  call disp('Λ', L, style='pad', advance='no')
    !  call disp('Λref', LL, style='pad', advance='no')

    !endif

    !call disp('Rel. error', err(1, :), fmt=fmt, style='pad', advance='no')
    !call disp('Abs. error', err(2, :), fmt=fmt, style='pad')
    !call assert(maxval(err(1, :)) < 1e-6, 'Maximum error threshold exceeded')

    !!call output('lambda.txt', L, digmax=16)
    !call output('data.txt', reshape(X, [size(X)]), digmax=16)

  end


  function eig_ref(X, n) result(L)
    real(real64), intent(in) :: X(:, :)
    integer, intent(in) :: n
    real(real64) :: Lreal(n), Limag(n)
    complex(real64) :: L(n)
    real(real64) :: Z(1, 1), H(n, n)
    integer :: info, i

    Lreal = 0.0d0
    Limag = 0.0d0
    H = eig_hessenberg_matrix(X)
    call dlahqr(.false., .false., n, 1, n, H, n, Lreal, Limag, 1, n, Z, 1, info)
    call assert(info == 0, 'dlahqr failed')
    L = [(complex(Lreal(i), Limag(i)), i=1,n)]
  end function

  subroutine read_input(fname, ndim, ntimes)
    character(len=*), intent(in) :: fname
    integer, intent(out) :: ndim, ntimes
    integer :: io, rc
    namelist /config/ ndim, ntimes

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
