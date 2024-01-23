program scratch
  use iso_fortran_env
  implicit none

  integer :: i, d, j, n, rank, fact
  integer, allocatable :: pi(:)

  rank = 50
  n = 5

  allocate(pi(n), source=0)

  pi(n) = 1
  fact = 1

  do j = 1, n - 1

    fact = fact * j

    d = mod(rank, (j + 1) * fact) / fact
    rank = rank - d * fact

    pi(n - j) = d + 1

    do i = n - j + 1, n
      if (pi(i) > d) pi(i) = pi(i) + 1
    end do
  end do

  print *, pi

end program
