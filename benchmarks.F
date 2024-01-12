module benchmarks
  use iso_fortran_env
  implicit none
  private
  public :: pseudo_random_lower_hessenberg, benchmark_data

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
      X(j:, j) = normal_deviate(n - j + 1)
    enddo

    do j=2, n
      X(j - 1, j) = norm2(X(j:,j))
    enddo

  end function

  function normal_deviate(n, mu, sigma)
    integer, intent(in) :: n
    real(real64), value, optional :: mu, sigma
    real(real64), allocatable :: U(:,:), normal_deviate(:)
    real(real64), parameter :: pi = 3.141592653589793d0

    if (.not.present(mu)) mu = 0.0d0
    if (.not.present(sigma)) sigma = 1.0d0

    allocate(U(n,2), source=0.d0)
    call random_number(U)

    normal_deviate = mu + sigma*sqrt(-2*log(U(:,1)))*cos(2.0d0*pi*U(:,2))
  end function

end module
