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
    private
    procedure, public :: ok
    procedure, public :: skip
    procedure :: crash
    procedure :: write
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
  character(:), allocatable :: start_heading

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

    call self%write()
    if (.not.outcome) call backtrace()
  end

  subroutine skip(self)
    class(test_run) :: self
    self%outcome = .true.
    self%message = '# SKIP'
    call self%write()
  end subroutine

  subroutine crash(self)
    class(test_run) :: self
    self%outcome = .false.
    self%message = 'The test run crashed'
    call self%write()
  end subroutine

  subroutine write(self)
    class(test_run) :: self
    write(output_unit, edit_descriptor()) self%status(), self%test_case%test_id, ' - ', self%description()
  end

  function edit_descriptor() result(fmt)
    character(128) :: fmt
    write(fmt, '("I",I0)') ceiling(log10(1.0d0 * num_tests)) + 1
    fmt = '(A, X, '//trim(fmt)//' A, A)'
  end function

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
    character(128) :: error_msg

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
    start_heading = heading
  end subroutine

  subroutine test_suite_run___(heading)
    integer :: i
    logical :: ok = .false., all_ok = .true.
    type(test_run) :: run
    type(test_case) :: it
    character(*), intent(in) :: heading

    ! Cmdline
    integer :: nargs, rc
    character(:), allocatable :: command_line, arg

    allocate(character(128)::command_line)
    allocate(character(128)::arg)
    nargs = command_argument_count()

    call assert(nargs < 2, 'Expected zero or one arguments')

    if (nargs == 1) then

      call get_command_argument(1, arg)
      arg = arg(1:len(trim(arg)))

      do i = 1, num_tests
        it = test_cases(i)
        if (it%name == arg) then
          it = test_cases(i)
          run = test_run(test_case=it)

          call it%assertion(run)
          ok = run%outcome
          ! NB: The stop and error stop intrinsics output noise
          if (.not.ok) call exit(1)
          if (ok) call exit(0)
        endif
      enddo

      call assert(i <= num_tests, 'No such test: '//arg)

    else if (nargs == 0) then

      write(output_unit, '(A, A)') 'Start: ', start_heading
      write(output_unit, '(80("-"))')
      write(output_unit, '(I0,A,I0)') 1, '..', num_tests

      call signal(11, handler_print)
      call signal(8, handler_print)

      call get_command_argument(0, arg)
      arg = trim(arg(1:len(trim(arg))))

      do i = 1, num_tests
        it = test_cases(i)
        write(command_line, '(A,X,A)') arg, it%name
        call execute_command_line(command_line, wait=.true., exitstat=rc)
        if (rc > 1) then
          run = test_run(test_case=it)
          call run%crash()
        endif
        if (rc /= 0) all_ok = .false.
      enddo

      write(output_unit, '(80("-"))')
      write(output_unit, '(A, A)') 'End: ', heading
      call assert(all_ok, 'There were test failures')
    endif

  end subroutine

   ! POSIX.1-2017:  void (*func)(int)
  subroutine handler_print(signum) bind(C)
    use iso_c_binding, only: c_int
    integer(c_int), value :: signum
    print *, 'handler_print invoked with signum =', signum
  end subroutine

end module
