module benchmarks
  use iso_fortran_env
  implicit none
  private
  public :: pseudo_random_lower_hessenberg
contains

  function pseudo_random_lower_hessenberg(n) result(X)
    integer, intent(in) :: n 
    real(real64), allocatable :: X(:,:), Z(:)
    integer :: j
    allocate(Z(n*n-n*(n+1)/2), source=0.0d0)
    X = reshape(normal_deviate(n*(n+1)/2), [n,n], pad=Z)
    forall (j=2:n) X(j - 1, j) = norm2(X(j:, j)) ! χ2 superdiagonals
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
