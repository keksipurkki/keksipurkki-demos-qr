#define test(test_case) test_suite___(test_case, 'test_case')
#define test_suite_prepare() test_suite_prepare___(__FILE__)
#define test_suite_run() test_suite_run___(__FILE__)
#define assert(ok, message) assert___(__FILE__, __LINE__, ok, message)

module assertions
  use iso_fortran_env

  private
  public :: test_run
  public :: test_suite_prepare___, test_suite___, test_suite_run___
  public :: assert___

  type test_case
    procedure(assertion), nopass, pointer :: assertion
    character(:), allocatable :: name
    integer :: test_id
  end type

  type test_run
    type(test_case) :: test_case
    logical :: outcome = .false.
    character(:), allocatable :: message
  contains
    procedure :: ok
    procedure :: description
    procedure :: status
  end type

  abstract interface
    subroutine assertion(run)
      import :: test_run
      type(test_run), intent(inout) :: run
    end
  end interface

  integer :: num_tests = 0
  type(test_case), allocatable :: test_cases(:)

contains

  subroutine ok(self, outcome, message)
    class(test_run) :: self
    logical :: outcome
    character(*), optional, intent(in) :: message

    self%outcome = outcome

    if (present(message)) then
      self%message = message
    else
      self%message = ''
    endif

    write(output_unit, '(A,X,I0,A,A)') self%status(), self%test_case%test_id, ' - ', self%description()
    if (.not.outcome) call backtrace()
  end

  function description(self)
    class(test_run), intent(in) :: self
    character(:), allocatable :: description
    description = self%test_case%name
    if (len(self%message) > 0) then
      description = description // ' - ' // self%message
    endif
  end

  function status(self)
    class(test_run), intent(in) :: self
    character(:), allocatable :: status
    if (self%outcome) then
      status = 'ok'
    else
      status = 'not ok'
    endif
  end

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

    test_cases(num_tests) = test_case(assertion=it, test_id=num_tests, name=test_case_name)

  end

  subroutine test_suite_prepare___(heading)
    character(*), intent(in) :: heading
    write(output_unit, '(A, A)') 'Start: ', heading
    write(output_unit, '(80("-"))')
  end subroutine

  subroutine test_suite_run___(heading)
    integer :: i
    logical :: ok = .false., all_ok = .true.
    type(test_run) :: run
    type(test_case) :: it
    character(*), intent(in) :: heading

    write(output_unit, '(I0,A,I0)') 1, '..', num_tests

    do i = 1, num_tests

      it = test_cases(i)
      run = test_run(test_case=it)

      call it%assertion(run)
      ok = run%outcome
      if (.not.ok) all_ok = .false.
    enddo

    write(output_unit, '(80("-"))')
    write(output_unit, '(A, A)') 'End: ', heading
    call assert(all_ok, 'There were test failures')
  end subroutine

end module
