#include "assertions.F"

program perf
  use iso_fortran_env
  use eigenvalues
  use assertions
  use benchmarks, only: benchmark_data
  use benchmarks, only: pseudo_random_lower_hessenberg
  implicit none

  integer, parameter :: ntimes = 10, ndim = 500
  integer :: k
  integer :: info
  real(real64), allocatable :: H(:,:)
  complex(real64), allocatable :: L(:), Lref(:)
  real(real64) :: tic, toc, t(ntimes), tlapack, tavg

  H = eig_hessenberg_matrix(pseudo_random_lower_hessenberg(ndim))

  t = 0.0d0
  do k = 1, ntimes
    call cpu_time(toc)
    L = eig_hessenberg(H)
    call cpu_time(tic)
    t(k) = tic - toc
  enddo

  tavg = sum(t) / size(t)


  t = 0.0d0
  do k = 1, ntimes
    call cpu_time(toc)
    call eig_lapack(H, Lref, ndim, info)
    call cpu_time(tic)
    call assert(info == 0, 'dlahqr failed')
    t(k) = tic - toc
  enddo

  tlapack = sum(t) / size(t)

  write(output_unit, '("Average = ", G0)') tavg
  write(output_unit, '("Average = ", G0)') tlapack

  if (tavg < tlapack) then
    print *, 'Faster than LAPACK by a factor of', (tavg/tlapack)**(-1)
  else
    print *, 'Slower than LAPACK by a factor of', (tavg/tlapack)
  endif

contains

  subroutine eig_lapack(X, L, n, info)
    real(real64), intent(in) :: X(:, :)
    integer, intent(out) :: info
    real(real64), allocatable :: H(:,:)
    complex(real64), allocatable, intent(out) :: L(:)
    integer, intent(in) :: n
    real(real64) :: Lreal(n), Limag(n)
    real(real64) :: Z(1, 1)

    Lreal = 0.0d0
    Limag = 0.0d0
    H = X
    call my_dlahqr(.false., .false., n, 1, n, H, n, Lreal, Limag, 1, n, Z, 1, info)
    L = cmplx(Lreal, Limag, kind=real64)
  end

end program
