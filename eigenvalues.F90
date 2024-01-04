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
      real(real64), pointer :: sub(:)
      real(real64), allocatable :: a(:)
      real(real64), pointer :: S(:,:)
      integer :: i, m, mk, n, k, deflation

      m = size(X, 1)
      n = size(X, 2)
      k = 0

      if (.not.present(itermax)) itermax = 30*m*n

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
      allocate(a(m), source=0.0d0)

      ! X ~ H, Hessenberg reduction
      H = eig_hessenberg(X)

      do

        mk = size(H, 1)
#ifdef DEBUG
        call disp('H'//tostring(k)//' = ', H)
#endif

        k = k + 1

        if (k >= itermax) then
          exit
        endif

        if (mk >= 3) then
          H = eig_shifted_double_step_unrolled(H)
        else if (mk == 2) then
          L(1:2) = eig_trivial(H)
          exit
        else if (mk == 1) then
          L(1:1) = eig_trivial(H)
          exit
        endif

        ! Deflation (leading block)
        S => H(:3, :3)
        deflation = eig_deflation(S)
        if (deflation > 0) then
          S => S(:deflation, :deflation)
          L(mk - deflation + 1: mk) = eig_trivial(S)
          H = H(deflation + 1:, deflation + 1:)
          cycle
        endif

        ! Deflation (trailing block)
        S => H(mk - 2:, mk - 2:)
        deflation = eig_deflation(S)
        if (deflation > 0) then
          deflation = 3 - deflation ! Reverse
          S => S(3 - deflation + 1:, 3 - deflation + 1:)
          L(mk - deflation + 1: mk) = eig_trivial(S)
          H = H(:mk - deflation, :mk - deflation)
          cycle
        endif

      enddo

    end subroutine

    ! Given a 3x3 block extracted along the diagonal of an upper Hessenberg
    ! matrix, the function inspects its input for block diagonality, i.e.
    ! whether it is in Schur form.
    !
    ! The size of the biggest block is returned
    !
    pure function eig_deflation(S) result(n)
      real(real64), intent(in) :: S(3,3)
      real(real64) :: d
      integer :: n

      d = abs(S(1,1)) + abs(S(2,2))
      if (d == 0.0d0) d = 1e-11

      ! First subdiagonal element S_21 < eps*(|S_11|+|S_22|)
      if (d == abs(S(2,1)) + d) then
        n = 1
        return
      endif

      d = abs(S(2,2)) + abs(S(3,3))
      if (d == 0.0d0) d = 1e-11

      ! Second subdiagonal element S_32 < eps*(|S_22|+|S_33|)
      if (d == abs(S(3,2)) + d) then
        n = 2
        return
      endif

      ! Not in Schur form
      n = 0

    end function

    pure function eig_trivial(X) result(L)
      real(real64), intent(in) :: X(:,:)
      real(real64), allocatable :: L(:)
      integer :: n
      n = size(X, 1)
      if (n == 1) then
        L = eig_1(X)
      else if (n == 2) then
        L = eig_2(X)
      endif
    end function

    pure function eig_2(X) result(L)
      real(real64), intent(in) :: X(2,2)
      real(real64) :: L(2)
      real(real64) :: m, p, discriminant

      m = (X(1,1) + X(2,2)) / 2
      p = X(1,1) * X(2,2) - X(1,2) * X(2,1)
      discriminant = sqrt(m*m - p)
      L(1) = m + discriminant
      L(2) = m - discriminant
    end function

    pure function eig_1(X) result(L)
      real(real64), intent(in) :: X(1,1)
      real(real64) :: L(1)
      L(1) = X(1,1)
    end function

    ! Produces the vector `u` for a Householder reflection P = I - 2u*transpose(u)
    pure function eig_reflector(x, dim) result(u)
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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!  Slow version with explicit transformation matrices
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    pure function eig_shifted_double_step(H) result(Hk)
      real(real64), intent(in), target :: H(:,:)
      real(real64), allocatable :: Hk(:,:), u(:), P(:,:), Hshifted(:)
      real(real64), allocatable :: urow(:,:), ucol(:,:)
      real(real64) :: S(2,2)
      real(real64) :: trace, determinant
      integer :: m, n, k, r

      m = size(H, 1)
      n = size(H, 2)

      Hk = H

      if (m < 3) return

      ! Pick shifts from the eigenvalues of the trailing 2x2 block
      ! If the shifts are close to actual eigenvalues, the trailing subdiagonal
      ! elements can be shown to converge to zero
      S = H(m - 1:, n - 1:)
      trace = S(1,1) + S(2,2)
      determinant = S(1,1) * S(2,2) - S(1,2) * S(2,1)

      ! The first column of Hshifted = (H - λ_1*I)(H - λ_2*I)
      ! The arithmetic here does away with complications with complex
      ! arithmetic (λ_1 == conj(λ_2)).
      allocate(Hshifted(m), source=0.0d0)
      Hshifted(1) = H(1,1) * H(1,1) + H(1,2) * H(2,1) - trace * H(1,1) + determinant
      Hshifted(2) = H(2,1) * (H(1,1) + H(2,2) - trace)
      Hshifted(3) = H(2,1) * H(3,2)

      ! Householder reflection matrix
      u = eig_reflector(Hshifted, dim=1)
      urow = reshape(u, [1, size(u)])
      ucol = reshape(u, [size(u), 1])

      ! Apply shifts
      P = eye(m) - matmul(2 * ucol, urow)
      Hk = matmul(matmul(P, H), P)

      ! Restore upper Hessenberg structure by "bulge chasing"
      do k = 1, m - 2
        r = min(k + 3, m)
        P = eig_bulge_chaser(Hk, k, block_size=r - k)
        Hk = matmul(matmul(P, Hk), P)
      enddo

    end function

    ! A matrix that transforms column k of H into upper Hessenberg form
    pure function eig_bulge_chaser(H, k, block_size) result(P)
      real(real64), intent(in), target :: H(:,:)
      integer, intent(in) :: k, block_size
      integer :: m

      real(real64) :: PP(block_size, block_size), P(size(H, 1), size(H, 2))
      real(real64), allocatable :: u(:)
      real(real64), allocatable :: urow(:,:), ucol(:,:)

      u = eig_reflector(H(k + 1:k + block_size, k), dim=1)
      urow = reshape(u, [1, size(u)])
      ucol = reshape(u, [size(u), 1])
      PP = eye(block_size) - matmul(2 * ucol, urow)

      P = eye(size(H, 1))
      P(k + 1: k + 1 + block_size, k + 1: k + 1 + block_size) = PP

    end function

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!  Optimized version
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    pure function eig_shifted_double_step_unrolled(H) result(Hk)
      real(real64), intent(in) :: H(:,:)
      real(real64), allocatable, target :: Hk(:,:)
      integer, parameter :: block_size = 3, shift_size = 2

      real(real64), allocatable :: u(:), urow(:,:), ucol(:,:)
      real(real64), pointer :: X(:,:)

      real(real64) :: S(shift_size, shift_size), Hshifted(block_size)
      real(real64) :: trace, determinant
      integer :: m, n, k, r

      m = size(H, 1)
      n = size(H, 2)

      Hk = H

      if (m < block_size) return

      ! If this is gibberish, read up on "Implicit Q theorem"

      ! Pick shifts from the eigenvalues of the trailing 2x2 block
      ! If the shifts are close to actual eigenvalues, the two
      ! trailing subdiagonal elements can be shown to converge to zero
      S = H(m - 1:, n - 1:)
      trace = S(1,1) + S(2,2)
      determinant = S(1,1) * S(2,2) - S(1,2) * S(2,1)

      ! The first three elements of the first column of Hshifted = (H - λ_1*I)(H - λ_2*I)
      ! The eigenvalues can be complex-valued. However, in the 2x2 case λ_1 == conj(λ_2)
      ! which allows us to work in real arithmetic as follows:
      Hshifted(1) = H(1,1) * H(1,1) + H(1,2) * H(2,1) - trace * H(1,1) + determinant
      Hshifted(2) = H(2,1) * (H(1,1) + H(2,2) - trace)
      Hshifted(3) = H(2,1) * H(3,2)

      ! Householder reflection
      ! The full refection has the block structure
      !
      ! P = [[PP, 0],
      !      [0,  I]]
      !
      ! where PP is the 3x3 Householder reflection matrix computed from Hshifted
      call eig_reflect(Hk, Hshifted, 1, block_size)

      ! Restore upper Hessenberg structure by "bulge chasing"
      do k = 1, m - 2
        r = min(m - k, block_size)
        call eig_reflect(Hk, Hk(k + 1:k + r, k), k + 1, k + r, k)
      enddo
    end function

    pure subroutine eig_reflect(H, vec, start, end, offset)
      real(real64), intent(inout), target :: H(:,:)
      integer, intent(in) :: start, end
      integer, value, optional :: offset
      real(real64), intent(in) :: vec(:)
      real(real64), pointer :: X(:,:)
      real(real64), allocatable :: u(:), urow(:,:), ucol(:,:)

      if (.not.present(offset)) offset = 1

      u = eig_reflector(vec, dim=1)
      urow = reshape(u, [1, size(u)])
      ucol = reshape(u, [size(u), 1])

      X => H(start:end, offset:)
      X = X - matmul(2 * ucol, matmul(urow, X))
      X => H(offset:, start:end)
      X = X - matmul(matmul(X, 2 * ucol), urow)
    end

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
        call eig_reflect(H, H(j + 1:, j), j + 1, n)
      enddo

    end function

    pure function eye(n)
      integer, intent(in) :: n
      integer :: i
      real(real64) :: eye(n,n)
      eye = 0.0d0
      forall (i=1:n) eye(i,i) = 1.0d0
    end function

end module
