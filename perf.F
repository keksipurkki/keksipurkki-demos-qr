#include "assertions.F"

program perf
  use iso_fortran_env
  use eigenvalues
  use assertions
  use benchmarks, only: benchmark_data
  use benchmarks, only: pseudo_random_lower_hessenberg
  implicit none

  integer, parameter :: ntimes = 10, ndim = 500
  integer :: k, info = 0
  real(real64), allocatable :: H(:,:)
  complex(real64), allocatable :: L(:)
  real(real64) :: tic, toc, t(ntimes)

  H = eig_hessenberg_matrix(pseudo_random_lower_hessenberg(ndim))

  do k = 1, ntimes
    call cpu_time(toc)
    L = eig_hessenberg(H, info)
    call cpu_time(tic)
    call assert(info == 0, 'No convergence')
    t(k) = tic - toc
  enddo

  write(output_unit, '("Average = ", G0)') sum(t) / size(t)

end program
