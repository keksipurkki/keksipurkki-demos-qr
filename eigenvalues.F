module eigenvalues
  use iso_fortran_env
  use display
  implicit none

contains

  recursive function eig(X, itermax) result(L)
    real(real64), intent(in) :: X(:, :)
    integer, value, optional :: itermax
    real(real64), allocatable :: L(:)

    real(real64), allocatable, target :: H(:, :)
    integer :: m, n, k, i, j
    real(real64), allocatable :: sub(:), shifts(:)

    m = size(X, 1)
    n = size(X, 2)

    if (.not. present(itermax)) itermax = 30 * m * n

    if (.not. allocated(L)) then
      allocate (L(m), source=0.0d0)
    end if

    if (m /= n) then
      call disp('Expected a square matrix')
      return
    end if

    if (itermax <= 0) then
      call disp('Expected a positive value for maximum number of iterations')
      return
    end if

    H = X

!#ifdef _DEBUG
!    call disp('H'//tostring(k, fmt='I3.3'), H, style='pad')
!    call disp()
!#endif

    if (m == 2) then
      L = eig_trivial(H)
      return
    end if

    if (m == 1) then
      L = eig_trivial(H)
      return
    end if

    sub = eig_truncated_subdiagonal(H)

    ! Subdivide the problem at the subdiagonal entries
    do i = size(sub), 1, -1
      if (sub(i) == 0.0d0) then
        L(:i) = eig(H(:i, :i))
        L(i + 1:) = eig(H(i + 1:, i + 1:))
        return
      end if
    end do

    ! Perform QR steps until the problem splits
    do k = 1, itermax

      shifts = eig_shift_vector(H)
      j = eig_shift_index(H, shifts)
      H(j:,j:) = eig_shifted_double_step(H(j:,j:), shifts)

      sub = eig_truncated_subdiagonal(H)

      if (any(sub == 0.0d0)) then

#ifdef _DEBUG
        call diagnostics(k, itermax, shape(H))
#endif
        exit
      end if
    end do

    if (k >= itermax) then
      call disp('Max iterations exceeded')
      call diagnostics(k, itermax, shape(H))
    end if

    L = eig(H)

  end

  pure function eig_shift_vector(H)
    real(real64), intent(in) :: H(:,:)
    integer, parameter :: block_size = 3, shift_size = 2
    real(real64) :: S(shift_size, shift_size)
    real(real64) :: eig_shift_vector(block_size)
    real(real64) :: trace, determinant
    integer :: m, n

    m = size(H, 1)
    n = size(H, 2)

    ! If this is gibberish, read up on "Implicit Q theorem"

    ! Pick shifts from the eigenvalues of the trailing 2x2 block
    ! If the shifts are close to actual eigenvalues, the two
    ! trailing subdiagonal elements can be shown to converge to zero
    S = H(m - 1:, n - 1:)
    trace = S(1, 1) + S(2, 2)
    determinant = S(1, 1) * S(2, 2) - S(1, 2) * S(2, 1)

    ! The first three elements of the first column of (H - λ_1*I)(H - λ_2*I)
    ! The eigenvalues can be complex-valued. However, in the 2x2 case λ_1 == conj(λ_2)
    ! which allows us to work in real arithmetic as follows:
    eig_shift_vector(1) = H(1, 1) * H(1, 1) + H(1, 2) * H(2, 1) - trace * H(1, 1) + determinant
    eig_shift_vector(2) = H(2, 1) * (H(1, 1) + H(2, 2) - trace)
    eig_shift_vector(3) = H(2, 1) * H(3, 2)
  end

  pure function eig_shift_index(H, u) result(indx)
    integer, parameter :: block_size = 3, shift_size = 2
    real(real64), intent(in) :: H(:,:), u(block_size)
    integer :: indx
    indx = 1
  end

  ! Subdiagonal of `H` with negligible entries set to zero
  !
  ! The converge criterion involving two adjacent diagonal element is a heuristic
  ! proposed by Wilkinson. The subdiagonal elements `S_i,i+1` will be considered to
  ! equal zero if `abs(S_i,i+1) < eps*(abs(S_i,i) + abs(S_i+1,i+1)) <= eps*norm(S)`.
  !
  ! Reference: Ahues & Tisseur (LAWN 122, 230: 1997)
  function eig_truncated_subdiagonal(H)
    real(real64), intent(in), contiguous, target :: H(:, :)
    real(real64), pointer :: sub(:), diag(:)
    real(real64), allocatable :: thrsd(:), adjacent(:, :)
    real(real64), allocatable :: eig_truncated_subdiagonal(:)

    diag => diagonal(H)
    sub => diagonal(H, 1)
    adjacent = reshape([diag(2:), diag(1:)], [2, size(sub)], order=[2, 1])
    thrsd = sum(abs(adjacent), 1)

    allocate (eig_truncated_subdiagonal(size(sub)))

    where (thrsd == thrsd + abs(sub))
      eig_truncated_subdiagonal = 0.0d0
    elsewhere
      eig_truncated_subdiagonal = sub
    end where

  end function

  function diagonal(H, offset) result(s)
    real(real64), intent(in), contiguous, target :: H(:, :)
    integer, value, optional :: offset
    real(real64), pointer :: tmp(:), s(:)
    tmp(1:size(H)) => H
    if (.not. present(offset)) offset = 0
    s => tmp(1 + offset :: size(H, 1) + 1)
  end function

  pure function eig_trivial(X) result(L)
    real(real64), intent(in) :: X(:, :)
    real(real64), allocatable :: L(:)
    integer :: n
    n = size(X, 1)

    if (n == 1) then
      L = eig_1(X)
    else if (n == 2) then
      L = eig_2(X)
    end if
  end function

  pure function eig_2(X) result(L)
    real(real64), intent(in) :: X(2, 2)
    real(real64) :: L(2)
    real(real64) :: m, p, discriminant

    m = (X(1, 1) + X(2, 2)) / 2
    p = X(1, 1) * X(2, 2) - X(1, 2) * X(2, 1)
    discriminant = sqrt(m * m - p)
    L(1) = m + discriminant
    L(2) = m - discriminant
  end function

  pure function eig_1(X) result(L)
    real(real64), intent(in) :: X(1, 1)
    real(real64) :: L(1)
    L(1) = X(1, 1)
  end function

  ! Produces the vector `u` for a Householder reflection P = I - 2u*transpose(u)
  pure function eig_reflector(x, dim) result(u)
    real(real64), intent(in) :: x(:)
    integer, optional, value :: dim
    real(real64) :: u(size(x))
    real(real64) :: rho

    if (.not. present(dim)) dim = 1

    rho = -1 * sign(1.0d0, x(dim))

    u = x
    u(dim) = x(dim) - rho * norm2(x)
    u = u / norm2(u)

  end function

  pure function eig_shifted_double_step(H, u) result(Hk)
    real(real64), intent(in) :: H(:, :), u(3)
    real(real64), allocatable, target :: Hk(:, :)
    integer, parameter :: block_size = 3, shift_size = 2
    integer :: m, n, k, r

    m = size(H, 1)
    n = size(H, 2)

    Hk = H

    if (m /= n) return
    if (m < block_size) return

    ! Apply shifts implicitly via a Householder reflection
    !
    ! The full refection has the block structure
    !
    ! P = [[PP, 0],
    !      [0,  I]]
    !
    ! where PP = I - 2*u*transpose(u) is a 3x3 Householder reflection matrix
    call eig_reflect(Hk, u, 1, block_size)

    ! Restore upper Hessenberg structure by "bulge chasing"
    do k = 1, m - 2
      r = min(m - k, block_size)
      call eig_reflect(Hk, Hk(k + 1:k + r, k), k + 1, k + r, k)
    end do
  end function

  pure subroutine eig_reflect(H, vec, start, end, offset)
    real(real64), intent(inout), target :: H(:, :)
    integer, intent(in) :: start, end
    integer, optional, value :: offset
    real(real64), intent(in) :: vec(:)
    real(real64) :: u(size(vec))

    real(real64), pointer :: X(:, :)
    real(real64), allocatable :: urow(:, :), ucol(:, :)

    if (.not. present(offset)) offset = 1

    u = eig_reflector(vec, dim=1)
    urow = reshape(u, [1, size(u)])
    ucol = reshape(u, [size(u), 1])

    ! transpose(P) * H * P
    X => H(start:end, offset:)
    X = X - matmul(2 * ucol, matmul(urow, X))
    X => H(offset:, start:end)
    X = X - matmul(matmul(X, 2 * ucol), urow)
  end

  ! Given an nxn matrix A, the function returns the upper Hessenberg form of A
  function eig_hessenberg(A) result(H)
    real(real64), intent(in) :: A(:, :)
    real(real64), allocatable, target :: H(:, :)
    integer :: j, n

    n = size(A, 1)
    H = A

    do j = 1, n - 2 ! P1, P2, P3...Pn-2 Householder reflections
      call eig_reflect(H, H(j + 1:, j), j + 1, n)
    end do

  end function

  pure function eye(n)
    integer, intent(in) :: n
    integer :: i
    real(real64) :: eye(n, n)
    eye = 0.0d0
    forall (i=1:n) eye(i, i) = 1.0d0
  end function

  pure function zeros(n)
    integer, intent(in) :: n
    real(real64) :: zeros(n)
    zeros = 0.0d0
  end function

  subroutine diagnostics(k, itermax, s)
    integer, intent(in) :: k, itermax, s(2)
    integer :: width
    width = nint(log10(1.0d0 * itermax)) + 1
    call disp('k = ', k, advance='no', fmt='I'//tostring(width)//'.'//tostring(width))
    call disp('itermax = ', itermax, advance='no')
    call disp('shape = ', s, orient='row', advance='yes')
  end subroutine

end module
