program test_pcre

  use, intrinsic :: iso_c_binding
  use pcre_module

  implicit none

  type token_type
     character(len=:), allocatable :: token
  end type token_type

  type(token_type), dimension(:), allocatable :: tokens

  character(len=:), allocatable :: pattern
  character(len=:), allocatable :: subject

  ! pattern = "[ \s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""|;.*|[^ \s\[\]{}('""`,;)]*)"
  ! subject = "(+ 2 (* 3 4))"

  ! character(len=*), parameter :: pattern = "[ ,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""|;.*|[^ \[\]{}('""`,;)]*)"
  ! character(len=*), parameter :: subject = "(+ 2 (* 3 4))"

  call test_valid_pattern("^foam", "foam", "foam")
  call test_invalid_pattern("^foam", "bath foam")

  call test_valid_pattern("finish$", "finish", "finish")

  pattern = "abc(.*)ghi"
  subject = "abcdefghi"

  print('(A,A,A,A)'), "Searching ", subject, " for ", pattern
  print*, "Should find: 'def:'"

  tokens = tokeniser(pattern, subject)

  call print_tokens(tokens)

  deallocate(tokens)

  pattern = "abc.*(ghi)"
  subject = "abcdefghi"

  print('(A,A,A,A)'), "Searching ", subject, " for ", pattern
  print*, "Should find: 'ghi:'"

  tokens = tokeniser(pattern, subject)

  call print_tokens(tokens)

  deallocate(tokens)
  
  pattern = "abc(def)(ghi)"
  subject = "abcdefghi"

  print('(A,A,A,A)'), "Searching ", subject, " for ", pattern
  print*, "Should find: 'def:ghi:'"

  tokens = tokeniser(pattern, subject)

  call print_tokens(tokens)

  deallocate(tokens)

  pattern = "[ ]*([a-z]*)"
  subject = "    super    doge"

  print('(A,A,A,A)'), "Searching ", subject, " for ", pattern
  print*, "Should find: 'super:doge:'"

  tokens = tokeniser(pattern, subject)

  call print_tokens(tokens)

  deallocate(tokens)

  pattern = "([0-9]+)"
  subject = "abc123def456"

  print('(A,A,A,A)'), "Searching ", subject, " for ", pattern
  print*, "Should find: '123:456:'"

  tokens = tokeniser(pattern, subject)

  call print_tokens(tokens)

  deallocate(tokens)

  pattern = "(\d+)"
  subject = "abc123def456"

  print('(A,A,A,A)'), "Searching ", subject, " for ", pattern
  print*, "Should find: '123:456:'"

  tokens = tokeniser(pattern, subject)

  call print_tokens(tokens)

  deallocate(tokens)

contains

  subroutine test_valid_pattern(pattern, valid_match, expected)
    character(len=:), allocatable, intent(in) :: pattern
    character(len=:), allocatable, intent(in) :: valid_match
    character(len=:), allocatable, intent(in) :: expected

    logical :: status

    type(token_type), dimension(:), allocatable :: tokens

    print('(A,A,A,A)'), "Searching ", valid_match, " for ", pattern
    print('(A,A)'), "Should match: ", expected

    tokens = tokeniser(pattern, valid_match)

    call print_tokens(tokens)

  end subroutine test_pattern
  
  subroutine test_invalid_pattern(pattern, invalid_match)
    character(len=:), allocatable, intent(in) :: pattern
    character(len=:), allocatable, intent(in) :: invalid_match

    logical :: status

    type(token_type), dimension(:), allocatable :: tokens

    print('(A,A,A,A)'), "Searching ", invalid_match, " for ", pattern
    print('(A)'), "Should not match"

    tokens = tokeniser(pattern, subject)

    call print_tokens(tokens)

    print('(A)'), "--------------------"

  end subroutine test_pattern

  subroutine print_tokens(tokens)
    type(token_type), dimension(:), intent(in) :: tokens
    integer :: i
    
    do i=1, size(tokens)
       write(*,'(A,":")', advance='no') tokens(i)%token
    end do
    print*, ""

  end subroutine print_tokens
  
  !> Finds all instances of pattern in
  !> subject, but only the first group from each instance
  function tokeniser(pattern, subject) result(tokens)

    use, intrinsic :: iso_c_binding
    use pcre_module

    implicit none

    character(len=*), intent(in) :: pattern, subject
    type(token_type), dimension(:), allocatable :: tokens

    type(pcre_type) :: regex

    integer, parameter :: ovecsize = 30
    integer, dimension(0:ovecsize-1) :: ovector

    integer :: error
    integer :: total_matches

    regex = pcre_compile(pattern, 0)

    if (.not. c_associated(regex%regex)) then
       print*,"PCRE compilation failed"
       stop 1
    end if

    error = pcre_exec(regex, c_null_ptr, subject, 0, 0, ovector)

    if (error < 0) then
       return
    end if

    total_matches = 1
    allocate(tokens(total_matches))

    block
      integer :: substring_start
      integer :: substring_end

      substring_start = ovector(2) + 1
      substring_end = ovector(3)
      tokens(1) = token_type(subject(substring_start:substring_end))
    end block

    block
      integer :: options
      integer :: start_offset

      do
         options = 0
         start_offset = ovector(3)

         if (ovector(2) == ovector(3)) then
            if (ovector(2) == len(subject)) exit
         end if

         error = pcre_exec(regex, c_null_ptr, subject, start_offset, options, ovector)

         if (error == PCRE_ERROR_NOMATCH) then
            if (options == 0) exit
            ovector(1) = start_offset + 1
            continue
         end if

         if (error < 0) then
            print('(A,I0)'), "Matching error ", error
            stop 1
         end if

         block
           integer :: substring_start
           integer :: substring_end
           type(token_type) :: found_token

           total_matches = total_matches + 1
           substring_start = ovector(2) + 1
           substring_end = ovector(3)
           found_token = token_type(subject(substring_start:substring_end))
           tokens = [tokens, found_token]
         end block
      end do
    end block

  end function tokeniser


end program test_pcre
