#define test(test_case) test_suite___(test_case, 'test_case')
#define test_suite_start() test_suite_start___(__FILE__)
#define test_suite_end() test_suite_end___(__FILE__)
#define assert(ok, message) assert___(__FILE__, __LINE__, ok, message)

module assertions
  use iso_fortran_env

  private
  public :: test_suite_start___, test_suite___, test_suite_end___
  public :: assert___

  abstract interface
    logical function assertion()
    end
  end interface

  type test_case
    procedure(assertion), nopass, pointer :: assertion
    character(:), allocatable :: name
  end type

  integer :: num_tests = 0
  type(test_case), allocatable :: test_cases(:)

contains

  subroutine assert___(fname, linenr, ok, message)
    character(*), intent(in) :: fname, message
    integer, intent(in) :: linenr
    logical, intent(in) :: ok
    character(80) :: error_msg

    if (ok) then
      return
    else
      write(error_msg, '(3(G0,:,":"))') fname, linenr, message
      error stop trim(error_msg)
    end if
  end subroutine

  subroutine test_suite___(it, test_case_name)
    procedure(assertion) :: it
    character(*), intent(in) :: test_case_name
    type(test_case), allocatable :: copy(:)

    num_tests = num_tests + 1

    if (.not.allocated(test_cases)) then
      allocate(test_cases(10))
    endif

    if (num_tests > size(test_cases)) then
      copy = test_cases
      deallocate(test_cases)
      allocate(test_cases(2*num_tests))
      test_cases = copy
    endif

    test_cases(num_tests) = test_case(it, test_case_name)

  end

  subroutine test_suite_start___(heading)
    character(*), intent(in) :: heading
    write(output_unit, '(A, A)') 'Start: ', heading
    write(output_unit, '(80("-"))')
  end subroutine

  subroutine test_suite_end___(heading)
    integer :: i
    logical :: ok = .false., all_ok = .true.
    type(test_case) :: it

    character(*), intent(in) :: heading

    write(output_unit, '(I0,A,I0)') 1, '..', num_tests

    do i = 1, num_tests

      it = test_cases(i)

      ok = it%assertion()

      if (ok) then
        write(output_unit, '(A,X,I0,A,A)') 'ok', i, ' - ', it%name
      else
        write(output_unit, '(A,X,I0,A,A)') 'not ok', i, ' - ', it%name
      endif

      if (.not.ok) all_ok = .false.
    enddo

    write(output_unit, '(80("-"))')
    write(output_unit, '(A, A)') 'End: ', heading
    call assert(all_ok, 'There were test failures')
  end subroutine

end module
