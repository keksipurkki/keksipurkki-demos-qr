module benchmarks
  use iso_fortran_env
  implicit none
  private
  public :: benchmark_data
  public :: pseudo_random_lower_hessenberg
  public :: pseudo_random_positive_definite
  public :: pseudo_random_triangular
  public :: pseudo_random_orthogonal
  public :: pseudo_random_permutation

  abstract interface
    function benchmark_data(n)
      use iso_fortran_env
      integer, intent(in) :: n
      real(real64), allocatable :: benchmark_data(:, :)
    end function
  end interface
contains

  ! Kreher & Stinson (1998) https://doi.org/10.1201/9781003068006
  function pseudo_random_permutation(n) result(X)
    integer, intent(in) :: n
    integer :: rank, i, j, fact, d
    real(real64), allocatable :: X(:, :)
    integer, allocatable :: pi(:)
    real(real64) :: u

    allocate (X(n, n), source=0.0d0)
    allocate (pi(n), source=0)

    call random_number(u)

    fact = product([(i, i=1, n)])
    rank = floor(u * (fact - 1))

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

    do i = 1, n
      X(pi(i), i) = 1.0d0
    end do

  end

  function pseudo_random_lower_hessenberg(n) result(X)
    integer, intent(in) :: n
    real(real64), allocatable :: X(:, :)
    integer :: j

    allocate (X(n, n), source=0.0d0)

    do j = 1, n
      X(j:, j) = normal_variate(n - j + 1)
    end do

    do j = 2, n
      X(j - 1, j) = norm2(X(j:, j))
    end do

  end function

  function pseudo_random_positive_definite(n) result(X)
    integer, intent(in) :: n
    real(real64), allocatable :: X(:, :), Q(:, :)
    integer :: i

    allocate (X(n, n), source=0.0d0)

    do i = 1, n
      X(i, i) = 1.0d0 * i
    end do

    Q = pseudo_random_orthogonal(n)
    X = matmul(Q, matmul(X, transpose(Q)))

  end function

  function pseudo_random_triangular(n) result(X)
    integer, intent(in) :: n
    real(real64), allocatable :: X(:, :)
    integer :: j

    allocate (X(n, n), source=0.0d0)

    do j = 1, n
      X(j:, j) = normal_variate(n - j + 1)
    end do

    X = transpose(X)

  end function

  ! Diaconis, Shahshahani (1987) https://doi.org/10.1017/S0269964800000255
  recursive function pseudo_random_orthogonal(n) result(X)
    integer, intent(in) :: n
    real(real64), allocatable :: X(:, :), u(:), uu(:, :)
    real(real64), allocatable :: urow(:, :), ucol(:, :)
    integer :: i

    if (n == 2) then
      X = pseudo_random_orthogonal_2d()
      return
    end if

    allocate (X(n, n), source=0.d0)

    u = normal_variate(n)
    u = reflector(u / norm2(u))

    urow = reshape(u, [1, size(u)])
    ucol = reshape(u, [size(u), 1])
    uu = matmul(2.d0 * ucol, urow)

    X(1, 1) = 1.0d0
    X(2:, 2:) = pseudo_random_orthogonal(n - 1)

    do i = 1, n
      X(:, i) = X(:, i) - matmul(uu, X(:, i))
    end do

  end

  pure function reflector(x) result(u)
    real(real64), intent(in) :: x(:)
    real(real64) :: u(size(x))
    integer, parameter :: dim = 1

    u(dim) = 1.0d0 - x(dim)
    u(dim + 1:) = -x(dim + 1:)
    u = u / norm2(u)

  end function

  function pseudo_random_orthogonal_2d() result(X)
    real(real64), parameter :: pi = 3.141592653589793d0
    real(real64), allocatable :: X(:, :)
    real(real64) :: b, theta

    allocate (X(2, 2), source=0.d0)

    call random_number(b)
    call random_number(theta)

    theta = 2 * pi * theta
    b = sign(1.0d0, b - 0.5d0)

    X(:, 1) = [cos(theta), -b * sin(theta)]
    X(:, 2) = [sin(theta), b * cos(theta)]

  end

  function normal_variate(n, mu, sigma)
    integer, intent(in) :: n
    real(real64), value, optional :: mu, sigma
    real(real64), allocatable :: U(:, :), normal_variate(:)
    real(real64), parameter :: pi = 3.141592653589793d0

    if (.not. present(mu)) mu = 0.0d0
    if (.not. present(sigma)) sigma = 1.0d0

    allocate (U(n, 2), source=0.d0)
    call random_number(U)

    normal_variate = mu + sigma * sqrt(-2d0 * log(U(:, 1))) * cos(2.0d0 * pi * U(:, 2))
  end function

end module
