module eigenvalues
  use iso_fortran_env
  use display
  implicit none

  contains

    subroutine eig(X, Q, L, itermax)
      real(real64), intent(in) :: X(:,:)
      integer, value, optional :: itermax
      real(real64), intent(out), allocatable :: Q(:,:)
      real(real64), intent(out), allocatable :: L(:)
      real(real64), allocatable, target :: H(:,:)
      real(real64), pointer :: S(:,:)
      integer :: m, mk, n, i, k
      real(real64), parameter :: eps = epsilon(1.0_real64)

      m = size(X, 1)
      n = size(X, 2)
      k = 0
      if (.not.present(itermax)) itermax = 1000

      if (m /= n) then
        call disp('Expected a square matrix')
        return
      endif

      if (itermax <= 0) then
        call disp('Expected a positive value for maximum number of iterations')
        return
      endif

      allocate(Q(m,m), source=0.0d0)
      allocate(L(m), source=0.0d0)

      ! X ~ H, Hessenberg reduction
      H = eig_hessenberg(X)

      do

        mk = size(H, 1)
        call disp('H'//tostring(k)//' = ', H)

        if (mk >= 3) then
          H = eig_shifted_double_step(H)
          !H = eig_step(H)
        else if (mk == 2) then
          L(1:2) = eig_2(H)
          exit
        else if (mk == 1) then
          L(1) = H(1,1)
          exit
        endif

        ! Deflation
        if (H(mk - 1, mk - 2) ** 2 <= eps) then
          L(mk - 1 : mk) = eig_2(H(mk - 1:, mk - 1:))
          H = H(:mk - 2, :mk - 2)
        end if

        k = k + 1

        if (k >= itermax) then
          exit
        endif

      enddo

    end subroutine

    ! H is assumed to be in upper Hessenberg form
    function eig_step(H) result(Hout)
      real(real64), intent(in) :: H(:,:)
      real(real64), allocatable, target :: Hout(:,:)
      real(real64), pointer :: X(:,:)
      real(real64), target, allocatable :: V(:,:)
      real(real64), allocatable :: u(:), vcol(:,:), vrow(:,:)

      integer :: j, n

      Hout = H

      n = size(Hout, 1)
      allocate(V(2, n - 1), source=0.d0)

      do j = 1, n - 1
        u = Hout(j:j+1, j)
        u(1) = u(1) + sign(1.d0, u(1)) * norm2(u)

        V(:,j) = u / norm2(u)

        vrow = reshape(V(:,j), [1, 2])
        vcol = reshape(V(:,j), [2, 1])

        X => Hout(j:j+1,:)
        X = X - 2 * matmul(vcol, matmul(vrow, X))

      enddo

      do j = 1, n - 1
        vrow = reshape(V(:,j), [1, 2])
        vcol = reshape(V(:,j), [2, 1])
        X => Hout(:, j:j+1)
        X = X - matmul(matmul(X, 2 * vcol), vrow)
      enddo

    end function

    ! Produces the vector `u` for a Householder reflection P = I - 2u*transpose(u)
    function eig_reflector(x, dim) result(u)
      real(real64), intent(in) :: x(:)
      integer, optional, value :: dim
      real(real64), allocatable :: u(:)
      real(real64) :: rho, length

      if (.not.present(dim)) dim = 1

      rho = -1*sign(1.0d0, x(dim))

      u = x
      u(dim) = x(dim) - rho * norm2(x)

      u = u / norm2(u)

    end function

    function eig_shifted_double_step(H) result(Hout)
      real(real64), intent(in), target :: H(:,:)
      real(real64), allocatable :: Hout(:,:), u(:), P(:,:), Hshifted(:)
      real(real64), allocatable :: urow(:,:), ucol(:,:)
      real(real64), pointer :: S(:,:), X(:)
      real(real64) :: trace, determinant
      integer :: m, n, k, r

      m = size(H, 1)
      n = size(H, 2)

      S => H(m - 1:, n - 1:)
      trace = S(1,1) + S(2,2)
      determinant = S(1,1) * S(2,2) - S(1,2) * S(2,1)

      ! The first column of Hshifted = (H - lambda_1*I)(H - lambda_2*I)
      ! where the lambda_i are the eigenvalues of the trailing 2x2 block of H
      allocate(Hshifted(m), source=0.0d0)
      Hshifted(1) = H(1,1)*H(1,1) + H(1,2) * H(2,1) - trace * H(1,1) + determinant
      Hshifted(2) = H(2,1) * (H(1,1) + H(2,2) - trace)
      Hshifted(3) = H(2,1) * H(3,2)

      !call disp('Shortcut = ', Hshifted(:3))
      !call disp('Full = ', matmul(H,H) - trace*H + determinant*eye(m))

      ! Householder reflection matrix
      u = eig_reflector(Hshifted, dim=1)
      urow = reshape(u, [1, size(u)])
      ucol = reshape(u, [size(u), 1])

      ! Apply shifts
      P = eye(m) - matmul(2 * ucol, urow)
      Hout = matmul(matmul(P, H), P)

      !call disp('P = ', P)
      !call disp('Bulging = ', Hout)
      !call disp('Reflected = ', matmul(P, Hshifted))

      ! Restore upper Hessenberg structure by "bulge chasing"
      do k = 1, m - 2
        r = min(k + 3, m)
        P = eig_bulge_chaser(Hout, k, block_size=r - k)
        Hout = matmul(matmul(P, Hout), P)
      enddo

    end function

    function eig_bulge_chaser(H, k, block_size) result(P)
      real(real64), intent(in), target :: H(:,:)
      real(real64), pointer :: Hblock(:)
      integer, intent(in) :: k, block_size
      integer :: m

      real(real64) :: PP(block_size, block_size), P(size(H, 1), size(H, 2))
      real(real64), allocatable :: u(:)
      real(real64), allocatable :: urow(:,:), ucol(:,:)

      Hblock => H(k + 1:k + block_size, k)

      !call disp('block size = ', block_size)
      !call disp('k = ', k)
      !call disp('H = ', H)
      !call disp('Hblock = ', Hblock)

      u = eig_reflector(Hblock, dim=1)
      urow = reshape(u, [1, size(u)])
      ucol = reshape(u, [size(u), 1])
      PP = eye(block_size) - matmul(2 * ucol, urow)
      !call disp('PP = ', PP)

      P = eye(size(H, 1))
      P(k + 1: k + 1 + block_size, k + 1: k + 1 + block_size) = PP

      !call disp('P = ', P)
      !call disp('Reflected = ', matmul(PP, Hblock))

    end function

    ! Given an nxn matrix A, the function returns the upper Hessenberg form of A
    function eig_hessenberg(A) result(H)
      real(real64), intent(in) :: A(:,:)
      real(real64), allocatable, target :: H(:,:)
      real(real64), allocatable :: urow(:,:), ucol(:,:), u(:)
      real(real64), pointer :: X(:,:)
      integer :: j, n

      n = size(A, 1)
      H = A

      do j = 1, n - 2 ! P1, P2, P3...Pn-2 Householder reflections
        u = eig_reflector(H(j + 1:, j), 1)
        urow = reshape(u, [1, size(u)])
        ucol = reshape(u, [size(u), 1])

        X => H(j + 1:, :)
        X = X - matmul(2 * ucol, matmul(urow, X))

        X => H(:, j + 1:)
        X = X - matmul(matmul(X, 2 * ucol), urow)
      enddo

    end function

    pure function eye(n)
      integer, intent(in) :: n
      integer :: i
      real(real64) :: eye(n,n)
      eye = 0.0d0
      forall (i=1:n) eye(i,i) = 1.0d0
    end function

    pure function eig_2(X) result(L)
      real(real64), intent(in) :: X(2,2)
      complex(real64) :: L(2), tmp
      real(real64) :: mu, p

      mu = (X(1,1) + X(2,2)) / 2
      p = X(1,1) * X(2,2) - X(1,2) * X(2,1)

      ! m + √(m² – p)
      L(1) = mu + sqrt(mu*mu - p)

      ! m - √(m² – p)
      L(2) = mu - sqrt(mu*mu - p)

      if (abs(L(1)) < abs(L(2))) then
        tmp = L(2)
        L(2) = L(1)
        L(1) = tmp
      endif

    end function

end module
