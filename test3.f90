! Test the Fortran PCRE wrapper
!
! Examples taken from
! https://github.com/niklongstone/regular-expression-cheat-sheet
program test_pcre

  use, intrinsic :: iso_c_binding
  use pcre_module

  implicit none

  type token_type
     character(len=:), allocatable :: token
  end type token_type

  ! Start of string or line
  call test_valid_pattern("^foam", "foam", "foam")
  call test_invalid_pattern("^foam", "bath foam")

  ! Start of string, any match mode
  call test_valid_pattern("\Afoam", "foam", "foam")
  call test_invalid_pattern("\Afoam", "bath foam")

  ! End of string or line
  call test_valid_pattern("finish$", "finish", "finish")
  call test_invalid_pattern("finish$", "finnish")

  ! End of string, any match mode
  call test_valid_pattern("finish\Z", "finish", "finish")
  call test_invalid_pattern("finish\Z", "finnish")

  ! ! Word boundary
  call test_valid_pattern("\bis\b", "This island is beautiful", "is")
  call test_invalid_pattern("\bis\b", "This island isn't beautiful")

  ! Not word boundary
  call test_valid_pattern("\Bland", "island", "land")
  call test_invalid_pattern("\Bland", "peninsula")

  ! Positive lookahead
  call test_valid_pattern("question(?=s)", "questions", "question")
  call test_invalid_pattern("question(?=s)", "question")

  ! Negative lookahead
  call test_valid_pattern("answer(?!s)", "answer", "answer")
  call test_invalid_pattern("answer(?!s)", "answers")

  ! Positive look-behind
  call test_valid_pattern("(?<=appl)e", "apple", "e")
  call test_invalid_pattern("(?<=appl)e", "orange")

  ! Negative look-behind
  call test_valid_pattern("(?<!goo)d", "mood", "d")
  call test_invalid_pattern("(?<!goo)d", "good")

  ! Digit
  call test_valid_pattern("\d+", "0101", "0101")
  call test_invalid_pattern("\d+", "string")

  ! Not a digit
  call test_valid_pattern("\D+", "string", "string")
  call test_invalid_pattern("\D+", "0101")

  ! Word character
  call test_valid_pattern("\w+", "string", "string")
  call test_invalid_pattern("\w+", "0101")

  ! Not a word character
  call test_valid_pattern("\W+", ".$?%", ".$?%")
  call test_invalid_pattern("\W+", "string")

contains

  subroutine test_valid_pattern(pattern, valid_match, expected)
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in) :: valid_match
    character(len=*), intent(in) :: expected

    logical :: status

    type(token_type), dimension(:), allocatable :: tokens

    print('(A,A,A,A)'), "Searching ", valid_match, " for ", pattern
    print('(A,A)'), "Should match: ", expected

    tokens = tokeniser(pattern, valid_match)

    if (size(tokens) > 0) then
       call print_tokens(tokens)
    end if
    
    print('(A)'), "--------------------"

  end subroutine test_valid_pattern
  
  subroutine test_invalid_pattern(pattern, invalid_match)
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in) :: invalid_match

    logical :: status

    type(token_type), dimension(:), allocatable :: tokens

    print('(A,A,A,A)'), "Searching ", invalid_match, " for ", pattern
    print('(A)'), "Should not match"

    tokens = tokeniser(pattern, invalid_match)

    if (size(tokens) > 0) then
       call print_tokens(tokens)
    end if
    
    print('(A)'), "--------------------"

  end subroutine test_invalid_pattern

  subroutine print_tokens(tokens)
    type(token_type), dimension(:), intent(in) :: tokens
    integer :: i
    
    do i=1, size(tokens)
       write(*,'(A," ")', advance='no') tokens(i)%token
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
       allocate(tokens(0))
       return
    end if

    error = pcre_exec(regex, c_null_ptr, subject, 0, 0, ovector)

    if (error < 0) then
       select case(error)
       case (PCRE_ERROR_NOMATCH)
          print*,"No match"
       case default
          print*,"Matching error ", error
       end select
       allocate(tokens(0))
       return
    end if

    if (error == 0) then
       error = ovecsize / 3
       print('(A,I0,A)'),"ovector only has room for ", error - 1, " captured substrings"
    end if

    total_matches = 1
    allocate(tokens(total_matches))

    block
      integer :: substring_start
      integer :: substring_end
      integer :: i

      do i = 0, error - 1
         substring_start = ovector(2*i) + 1
         substring_end = ovector(2*i + 1)
         tokens(i+1) = token_type(subject(substring_start:substring_end))
      end do
    end block

    block
      integer :: options
      integer :: start_offset

      do
         options = 0
         start_offset = ovector(1)

         if (ovector(0) == ovector(1)) then
            if (ovector(0) == len(subject)) exit
         end if

         error = pcre_exec(regex, c_null_ptr, subject, start_offset, options, ovector)

         if (error == PCRE_ERROR_NOMATCH) then
            if (options == 0) exit
            ovector(1) = start_offset + 1
            continue
         end if

         if (error < 0) then
            print('(A,I0)'), "Matching error ", error
            allocate(tokens(0))
            return
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
