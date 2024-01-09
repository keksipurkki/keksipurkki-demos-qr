include 'assertions.F'

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use iso_fortran_env
  implicit none

  call disp('-------------------------------------------------------------------')
  call assert_hessenberg_transformation_works();
  call disp('-------------------------------------------------------------------')

contains

  subroutine assert_hessenberg_transformation_works()
    real(real64) :: R(3,3)
    logical :: ok
    R = 1.0
    R(3,3) = 5.0
    ok = is_upper_hessenberg(eig_hessenberg(R))
    call assert(ok, 'Failure at '//__FILE__//':'//tostring(__LINE__))
    call disp('assert_hessenberg_transformation_works: 💚')
  end

  logical function is_upper_hessenberg(H)
    real(real64), intent(in), target :: H(:,:)
    real(real64), pointer :: d(:)
    integer :: m, n, i

    m = size(H, 1)
    n = size(H, 2)

    if (m /= n) then
      is_upper_hessenberg = .false.
      return
    end if

    do i=2, m - 2
      d => diagonal(H, i)
      if (any(d /= 0.0d0)) then
        is_upper_hessenberg = .false.
        return
      end if
    enddo

    is_upper_hessenberg = .true.

  end function

end program

