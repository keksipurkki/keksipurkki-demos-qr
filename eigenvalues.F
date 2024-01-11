module eigenvalues
  use iso_fortran_env
  use display
  use utils
  implicit none

  private

  public :: eig, eig_hessenberg, eig_hessenberg_matrix, eig_set

  integer, public, parameter :: EIG_ERR_RECTANGULAR_MATRIX = 2**0
  integer, public, parameter :: EIG_ERR_NO_CONVERGENCE = 2**1

contains

  subroutine eig_set(unit)
    integer, intent(in) :: unit
    call disp_set(unit=unit)
  end subroutine

  function eig(X, info, itermax) result(L)
    real(real64), intent(in) :: X(:, :)
    integer, intent(out) :: info
    integer, intent(in), optional :: itermax
    real(real64), allocatable :: L(:)
    L = eig_hessenberg(eig_hessenberg_matrix(X), info, itermax)
  end function

  recursive function eig_hessenberg(X, info, itermax) result(L)
    real(real64), parameter :: zero = 0.0_real64
    real(real64), intent(in), target :: X(:, :)
    integer, intent(out) :: info
    integer, intent(in), optional :: itermax
    real(real64), allocatable :: L(:)

    real(real64), allocatable, target :: shifts(:), H(:, :)
    real(real64), pointer :: Hsub(:,:), S(:,:), sub(:), diag(:)
    integer :: m, n, k, i, j, itermax_m

    allocate(H, mold=X)
    H = X
    m = size(H, 1)
    n = size(H, 2)

    if (.not.allocated(L)) then
      allocate (L(m), source=zero)
    endif

    if (m /= n) then
      info = EIG_ERR_RECTANGULAR_MATRIX
      return
    end if

    if (.not.present(itermax)) itermax_m = 30 * m * m
    if (present(itermax)) itermax_m = itermax

    if (itermax_m <= 0) then
      info = EIG_ERR_NO_CONVERGENCE
      return
    end if

    if (m == 2) then
      L = eig_trivial(H)
      return
    end if

    if (m == 1) then
      L = eig_trivial(H)
      return
    end if

    diag => diagonal(H)
    sub => diagonal(H, 1)

    ! Side-effect: overwrites subdiagonal of H
    sub = eig_truncated_subdiagonal(diag, sub)

    ! Upper triangular matrix
    if (all(sub == zero)) then
      L = diag
      return
    endif

    ! Subdivide the problem at the subdiagonal entries
    do i = size(sub), 1, -1
      if (sub(i) == zero) then
        L(:i) = eig_hessenberg(H(:i, :i), info, itermax_m)
        L(i + 1:) = eig_hessenberg(H(i + 1:, i + 1:), info, itermax_m)
        return
      end if
    end do

    ! Perform QR steps until the problem splits
    k = 0
    do while (all(sub /= zero))

      if (k >= itermax_m) then
        exit
      endif

      ! TODO: Exceptional shifts as in LAPACK
      diag => diagonal(H)
      S => H(m - 1:, m - 1:)
      j = eig_shift_index(H, diag, S)
      Hsub => H(j:, j:)

      S => Hsub(m - j:, m - j:)
      shifts = eig_shift_vector(Hsub, S)
      Hsub = eig_shifted_double_step(Hsub, shifts)

      diag => diagonal(H)
      sub => diagonal(H, 1)

      ! Side-effect: overwrites subdiagonal of H
      sub = eig_truncated_subdiagonal(diag, sub)

      k = k + 1
    end do

#ifdef _DEBUG
    call diagnostics('k =', k, itermax_m, shape(H))
#endif

    L = eig_hessenberg(H, info, itermax_m - k)

  end

  pure function eig_shift_vector(H, S)
    integer, parameter :: block_size = 3, shift_size = 2
    real(real64), intent(in) :: H(:, :)
    real(real64), intent(in) :: S(:,:)
    real(real64) :: eig_shift_vector(block_size)
    real(real64) :: trace, determinant

    ! If this is gibberish, read up on "Implicit Q theorem"

    ! Pick shifts from the eigenvalues of a 2x2 block
    ! If the shifts are close to actual eigenvalues, the subdiagonal elements
    ! can be shown to converge to zero
    trace = S(1, 1) + S(2, 2)
    determinant = S(1, 1) * S(2, 2) - S(1, 2) * S(2, 1)

    ! The first three elements of the first column of (H - λ_1*I)(H - λ_2*I)
    ! The eigenvalues can be complex-valued. However, in the 2x2 case λ_1 == conj(λ_2)
    ! which allows us to work in real arithmetic as follows:
    eig_shift_vector(1) = H(1, 1) * H(1, 1) + H(1, 2) * H(2, 1) - trace * H(1, 1) + determinant
    eig_shift_vector(2) = H(2, 1) * (H(1, 1) + H(2, 2) - trace)
    eig_shift_vector(3) = H(2, 1) * H(3, 2)
  end

  pure function eig_shift_index(H, D, S) result(indx)
    integer, parameter :: block_size = 3, shift_size = 2
    real(real64), intent(in) :: H(:, :), D(:)
    real(real64), pointer, intent(in) :: S(:, :)
    real(real64) :: u(block_size), ulp, rhs, lhs
    integer :: indx, m

    ulp = radix(1.0_real64) * epsilon(1.0_real64)
    m = size(H, 1)

    do indx = m - 2, 2, -1
      u = eig_shift_vector(H(indx:, indx:), S)
      lhs = abs(H(indx, indx - 1)) * (abs(u(2)) + abs(u(3)))
      rhs = abs(u(1)) * sum(abs(D(indx - 1:indx + 1)))
      if (lhs < ulp * rhs) exit
    end do

    indx = max(1, indx - 1)
  end

  ! Subdiagonal of `H` with negligible entries set to zero
  !
  ! The converge criterion involving two adjacent diagonal element is a heuristic
  ! proposed by Wilkinson. The subdiagonal elements `S_i,i+1` will be considered to
  ! equal zero if `abs(S_i,i+1) < eps*(abs(S_i,i) + abs(S_i+1,i+1)) <= eps*norm(S)`.
  !
  ! Reference: Ahues & Tisseur (LAWN 122, 230: 1997)
  pure function eig_truncated_subdiagonal(D, S)
    real(real64), intent(in) :: D(:), S(:)
    real(real64), allocatable :: thrsd(:), adjacent(:, :)
    real(real64), allocatable :: eig_truncated_subdiagonal(:)
    real(real64) :: ulp, tol

    ulp = radix(1.0_real64) * epsilon(1.0_real64)
    tol = tiny(1.0_real64) * (size(D) / ulp)

    adjacent = reshape([D(2:), D(1:)], [2, size(S)], order=[2, 1])
    thrsd = sum(abs(adjacent), 1)

    allocate (eig_truncated_subdiagonal(size(S)))

    where (abs(S) < ulp*thrsd)
      eig_truncated_subdiagonal = 0.0d0
    elsewhere
      eig_truncated_subdiagonal = S
    end where

    where (abs(eig_truncated_subdiagonal) < tol)
      eig_truncated_subdiagonal = 0.0d0
    end where

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

    m = (X(1, 1) + X(2, 2)) / 2.0d0
    p = X(1, 1) * X(2, 2) - X(1, 2) * X(2, 1)

    discriminant = sqrt(m*m - p)

    ! https://doi.org/10.1145/103162.103163
    if (m < 0.0d0) then
      L(1) = p / (m - discriminant)
      L(2) = m - discriminant
    else
      L(1) = m + discriminant
      L(2) = p / (m + discriminant)
    endif

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

    if (all(x == 0.d0)) then
      u = 0.0d0
      return
    endif

    rho = -1d0 * sign(1.0d0, x(dim))

    u = x
    u(dim) = x(dim) - rho * norm2(x)
    u = u / norm2(u)

  end function

  function eig_shifted_double_step(H, shifts) result(Hk)
    integer, parameter :: block_size = 3, shift_size = 2
    real(real64), intent(in) :: H(:, :), shifts(block_size)
    real(real64), allocatable :: Hk(:, :)
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
    call eig_reflect(Hk, shifts, 1, 1)

    ! Restore upper Hessenberg structure by "bulge chasing"
    do k = 1, m - 2
      r = min(m - k, block_size)
      call eig_reflect(Hk, Hk(k + 1:k + r, k), k + 1, k)
    end do

    ! Junk
    do k = 1, m - 2
      Hk(k + 2:, k) = 0.0d0
    enddo

  end function


  ! In-place computation of transpose(P)*H*P with respect to the following block structure
  !
  ! P = [[I, 0, 0],
  !      [0, PP, 0],
  !      [0, 0, I]]
  !
  ! The size of the leading identity matrix I is `col - 1`
  ! The size of the Householder reflection `PP` is `size(vec)`
  ! The trailing identity matrix takes the remaining space
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine eig_reflect(H, vec, row, col)
    real(real64), intent(inout), target :: H(:, :)
    integer, intent(in) :: row, col
    real(real64), intent(in) :: vec(:)

    real(real64) :: u(size(vec))
    real(real64), pointer :: X(:, :)
    real(real64), allocatable :: urow(:, :), ucol(:, :)
    integer :: start, end, offset

    start = row
    end = start + size(vec) - 1
    offset = col

    u = eig_reflector(vec, dim=1)
    urow = reshape(u, [1, size(u)])
    ucol = reshape(u, [size(u), 1])

    X => H(start:end, offset:)
    X = X - matmul(2.0d0 * ucol, matmul(urow, X))

    X => H(:,start:end)
    X = X - matmul(matmul(X, 2.0d0 * ucol), urow)
  end

  ! Given an nxn matrix A, the function returns the upper Hessenberg form of A
  function eig_hessenberg_matrix(A) result(H)
    real(real64), parameter :: ulp = radix(1.0_real64) * epsilon(1.0_real64)
    real(real64), intent(in) :: A(:, :)
    real(real64), allocatable, target :: H(:, :)
    integer :: k, n

    n = size(A, 1)
    H = A

    do k = 1, n - 2 ! P1, P2, P3...Pn-2 Householder reflections
      call eig_reflect(H, H(k + 1:, k), k + 1, k)
    end do

    ! Junk
    do k = 1, n - 2
      H(k + 2:, k) = 0.0d0
    enddo

  end function

  subroutine diagnostics(message, k, itermax, s)
    character(*), intent(in) :: message
    integer, intent(in) :: k, itermax, s(2)
    integer :: width
    width = nint(log10(1.0d0 * itermax)) + 1
    call disp(message, advance='no')
    call disp(k, advance='no', fmt='I'//tostring(width)//'.'//tostring(width))
    call disp('itermax = ', itermax, advance='no')
    call disp('shape = ', s, orient='row', advance='yes')
  end subroutine

end module
