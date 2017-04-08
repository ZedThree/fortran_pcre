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

  integer :: number_failed = 0

  ! Start of string or line
  number_failed = number_failed + test_valid_pattern("^foam", "foam", "foam")
  number_failed = number_failed + test_invalid_pattern("^foam", "bath foam")

  ! Start of string, any match mode
  number_failed = number_failed + test_valid_pattern("\Afoam", "foam", "foam")
  number_failed = number_failed + test_invalid_pattern("\Afoam", "bath foam")

  ! End of string or line
  number_failed = number_failed + test_valid_pattern("finish$", "finish", "finish")
  number_failed = number_failed + test_invalid_pattern("finish$", "finnish")

  ! End of string, any match mode
  number_failed = number_failed + test_valid_pattern("finish\Z", "finish", "finish")
  number_failed = number_failed + test_invalid_pattern("finish\Z", "finnish")

  ! Word boundary
  number_failed = number_failed + test_valid_pattern("\bis\b", "This island is beautiful", "is")
  number_failed = number_failed + test_invalid_pattern("\bis\b", "This island isn't beautiful")

  ! Not word boundary
  number_failed = number_failed + test_valid_pattern("\Bland", "island", "land")
  number_failed = number_failed + test_invalid_pattern("\Bland", "peninsula")

  ! Positive lookahead
  number_failed = number_failed + test_valid_pattern("question(?=s)", "questions", "question")
  number_failed = number_failed + test_invalid_pattern("question(?=s)", "question")

  ! Negative lookahead
  number_failed = number_failed + test_valid_pattern("answer(?!s)", "answer", "answer")
  number_failed = number_failed + test_invalid_pattern("answer(?!s)", "answers")

  ! Positive look-behind
  number_failed = number_failed + test_valid_pattern("(?<=appl)e", "apple", "e")
  number_failed = number_failed + test_invalid_pattern("(?<=appl)e", "orange")

  ! Negative look-behind
  number_failed = number_failed + test_valid_pattern("(?<!goo)d", "mood", "d")
  number_failed = number_failed + test_invalid_pattern("(?<!goo)d", "good")

  ! Character class definition
  number_failed = number_failed + test_valid_pattern("[axf]", "a, x, f", "a x f")
  number_failed = number_failed + test_invalid_pattern("[axf]", "b")

  ! Character class range
  number_failed = number_failed + test_valid_pattern("[a-c]", "a, b, c", "a b c")
  number_failed = number_failed + test_invalid_pattern("[a-c]", "d")

  ! Escape in character class
  number_failed = number_failed + test_valid_pattern("[a-f\.]", "a, b, .", "a b .")
  number_failed = number_failed + test_invalid_pattern("[a-f\.]", "g")

  ! Not in class
  number_failed = number_failed + test_valid_pattern("[^abc]", "de", "d e")
  number_failed = number_failed + test_invalid_pattern("[^abc]", "a")

  ! Any character except newline
  number_failed = number_failed + test_valid_pattern("b.ttle", "battle, bottle", "battle bottle")
  number_failed = number_failed + test_invalid_pattern("b.ttle", "bttle")

  ! Whitespace
  number_failed = number_failed + test_valid_pattern("good\smorning", "good morning", "good morning")
  number_failed = number_failed + test_invalid_pattern("good\smorning", "good.morning")

  ! Not whitespace
  number_failed = number_failed + test_valid_pattern("good\Smorning", "goodmorning", "goodmorning")
  number_failed = number_failed + test_invalid_pattern("good\Smorning", "good morning")
  
  ! Digit
  number_failed = number_failed + test_valid_pattern("\d+", "0101", "0101")
  number_failed = number_failed + test_invalid_pattern("\d+", "string")

  ! Not a digit
  number_failed = number_failed + test_valid_pattern("\D+", "string", "string")
  number_failed = number_failed + test_invalid_pattern("\D+", "0101")

  ! Word character
  number_failed = number_failed + test_valid_pattern("\w+", "string", "string")
  number_failed = number_failed + test_invalid_pattern("\w+", "0101")

  ! Not a word character
  number_failed = number_failed + test_valid_pattern("\W+", ".$?%", ".$?%")
  number_failed = number_failed + test_invalid_pattern("\W+", "string")

  ! Alternation
  number_failed = number_failed + test_valid_pattern("apple|orange", "orange, apple", "orange apple")
  number_failed = number_failed + test_invalid_pattern("apple|orange", "melon")

  ! Subpattern
  number_failed = number_failed + test_valid_pattern("foot(er|ball)", "footer, football", "footer er football ball")
  number_failed = number_failed + test_invalid_pattern("foot(er|ball)", "footpath")

  ! Non-capturing subpattern
  number_failed = number_failed + test_valid_pattern("foot(?:er|ball)", "footer, football", "footer football")
  number_failed = number_failed + test_invalid_pattern("foot(?:er|ball)", "footpath")

  ! One or more
  number_failed = number_failed + test_valid_pattern("ye+ah", "yeah, yeeeeeah", "yeah yeeeeeah")
  number_failed = number_failed + test_invalid_pattern("ye+ah", "yah")

  ! Zero or more
  number_failed = number_failed + test_valid_pattern("ye*ah", "yeah, yeeeeeah, yah", "yeah yeeeeeah yah")
  number_failed = number_failed + test_invalid_pattern("ye*ah", "yeh")

  ! Zero or one
  number_failed = number_failed + test_valid_pattern("yes?", "yes, ye", "yes ye")
  number_failed = number_failed + test_invalid_pattern("yes?", "yesss")

  ! n times exactly
  number_failed = number_failed + test_valid_pattern("fo{2}", "foo", "foo")
  number_failed = number_failed + test_invalid_pattern("fo{2}", "fo")

  ! Between n and m times
  number_failed = number_failed + test_valid_pattern("go{2,3}d", "good, goood", "good goood")
  number_failed = number_failed + test_invalid_pattern("go{2,3}d", "gooood")

  ! At least n times
  number_failed = number_failed + test_valid_pattern("go{2,}", "goo, gooo", "goo gooo")
  number_failed = number_failed + test_invalid_pattern("go{2,}", "go")

  print*, new_line('A') // new_line('A')
  if (number_failed > 0) then
     print*, "Some tests failed:", number_failed
  else
     print*, "All tests passed!"
  end if
  
contains

  integer function test_valid_pattern(pattern, valid_match, expected)
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in) :: valid_match
    character(len=*), intent(in) :: expected

    logical :: status

    type(token_type), dimension(:), allocatable :: tokens
    character(len=:), allocatable :: string

    print('(A,A,A,A,A)'), "Searching '", valid_match, "' for '", pattern, "'"
    print('(A,A)'), "Should match: ", expected

    tokens = tokeniser(pattern, valid_match)

    if (size(tokens) > 0) then
       string = print_tokens(tokens)
       if (string == expected) then
          test_valid_pattern = 0
       else
          test_valid_pattern = 1
          print*, "    *** FAILED ***"
       end if
    else
       test_valid_pattern = 1
       print*, "    *** FAILED ***"
    end if

    print('(A)'), "--------------------"

  end function test_valid_pattern
  
  integer function test_invalid_pattern(pattern, invalid_match)
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in) :: invalid_match

    logical :: status

    type(token_type), dimension(:), allocatable :: tokens
    character(len=:), allocatable :: string    

    print('(A,A,A,A,A)'), "Searching '", invalid_match, "' for '", pattern, "'"
    print('(A)'), "Should not match"

    tokens = tokeniser(pattern, invalid_match)

    if (size(tokens) > 0) then
       string = print_tokens(tokens)
       test_invalid_pattern = 1
       print*, "    *** FAILED ***"
    else
       test_invalid_pattern = 0
    end if
    
    print('(A)'), "--------------------"

  end function test_invalid_pattern

  function print_tokens(tokens) result(string)
    type(token_type), dimension(:), intent(in) :: tokens
    character(len=:), allocatable :: string
    integer :: i
    
    string = ""

    do i=1, size(tokens)
       write(*,'(A," ")', advance='no') tokens(i)%token
       string = string // tokens(i)%token // " "
    end do
    print*, ""

    string = trim(string)

  end function print_tokens
  
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

    total_matches = error
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
           integer :: i
           integer :: substring_start
           integer :: substring_end
           type(token_type) :: found_token

           do i = 0, error-1
              total_matches = total_matches + 1
              substring_start = ovector(2*i) + 1
              substring_end = ovector((2*i)+1)
              found_token = token_type(subject(substring_start:substring_end))
              tokens = [tokens, found_token]
           end do
         end block
      end do
    end block

  end function tokeniser

end program test_pcre
