module benchmarks
  use iso_fortran_env
  implicit none
  private
  public :: benchmark_data
  public :: pseudo_random_lower_hessenberg
  public :: rotation_matrix

  abstract interface
    function benchmark_data(n)
      use iso_fortran_env
      integer, intent(in) :: n
      real(real64), allocatable :: benchmark_data(:,:)
    end function
  end interface
contains

  function pseudo_random_lower_hessenberg(n) result(X)
    integer, intent(in) :: n
    real(real64), allocatable :: X(:,:)
    integer :: j

    allocate(X(n,n), source=0.0d0)

    do j=1, n
      X(j:, j) = normal_variate(n - j + 1)
    enddo

    do j=2, n
      X(j - 1, j) = norm2(X(j:,j))
    enddo

  end function

  ! Diaconis, Shahshahani (1987) https://doi.org/10.1017/S0269964800000255
  recursive function rotation_matrix(n) result(X)
    integer, intent(in) :: n
    real(real64), allocatable :: X(:,:), u(:), householder(:,:)
    real(real64), allocatable :: urow(:, :), ucol(:, :)
    integer :: i

    if (n == 2) then
      X = rotation_matrix_2d()
      return
    endif

    allocate(X(n,n), source=0.d0)
    allocate(householder(n,n), source=0.0d0)

    do i=1, n
      householder(i,i) = 1.d0
    enddo

    u = normal_variate(n)
    u = reflector(u / norm2(u))

    urow = reshape(u, [1, size(u)])
    ucol = reshape(u, [size(u), 1])

    householder = householder - matmul(2*ucol, urow)

    X(1, 1) = 1.0d0
    X(2:, 2:) = rotation_matrix(n - 1)
    X = matmul(householder, X)

  end

  pure function reflector(x) result(u)
    real(real64), intent(in) :: x(:)
    real(real64) :: u(size(x))
    integer, parameter :: dim  = 1

    u(dim) = 1.0d0 - x(dim)
    u(dim + 1:) = -x(dim + 1:)
    u = u / norm2(u)

  end function

  function rotation_matrix_2d() result(X)
    real(real64), parameter :: pi = 3.141592653589793d0
    real(real64), allocatable :: X(:,:)
    real(real64) :: b, theta

    allocate(X(2,2), source=0.d0)

    call random_number(b)
    call random_number(theta)

    theta = 2*pi*theta
    b = sign(1.0d0, b - 0.5d0)

    X(:, 1) = [cos(theta), -b*sin(theta)]
    X(:, 2) = [sin(theta), b*cos(theta)]

  end

  function normal_variate(n, mu, sigma)
    integer, intent(in) :: n
    real(real64), value, optional :: mu, sigma
    real(real64), allocatable :: U(:,:), normal_variate(:)
    real(real64), parameter :: pi = 3.141592653589793d0

    if (.not.present(mu)) mu = 0.0d0
    if (.not.present(sigma)) sigma = 1.0d0

    allocate(U(n,2), source=0.d0)
    call random_number(U)

    normal_variate = mu + sigma*sqrt(-2*log(U(:,1)))*cos(2.0d0*pi*U(:,2))
  end function

end module
