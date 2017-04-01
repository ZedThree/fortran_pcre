module pcre_module

  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env

  implicit none

  ! Possible errors
  integer, parameter :: PCRE_ERROR_NOMATCH          = -1
  integer, parameter :: PCRE_ERROR_NULL             = -2
  integer, parameter :: PCRE_ERROR_BADOPTION        = -3
  integer, parameter :: PCRE_ERROR_BADMAGIC         = -4
  integer, parameter :: PCRE_ERROR_UNKNOWN_OPCODE   = -5

  ! Request types for pcre_fullinfo
  integer, parameter :: PCRE_INFO_OPTIONS             = 0
  integer, parameter :: PCRE_INFO_SIZE                = 1
  integer, parameter :: PCRE_INFO_CAPTURECOUNT        = 2
  integer, parameter :: PCRE_INFO_BACKREFMAX          = 3
  integer, parameter :: PCRE_INFO_FIRSTBYTE           = 4
  integer, parameter :: PCRE_INFO_FIRSTTABLE          = 5
  integer, parameter :: PCRE_INFO_LASTLITERAL         = 6
  integer, parameter :: PCRE_INFO_NAMEENTRYSIZE       = 7
  integer, parameter :: PCRE_INFO_NAMECOUNT           = 8
  integer, parameter :: PCRE_INFO_NAMETABLE           = 9
  integer, parameter :: PCRE_INFO_STUDYSIZE           = 10
  integer, parameter :: PCRE_INFO_DEFAULT_TABLES      = 11
  integer, parameter :: PCRE_INFO_OKPARTIAL           = 12
  integer, parameter :: PCRE_INFO_JCHANGED            = 13
  integer, parameter :: PCRE_INFO_HASCRORLF           = 14
  integer, parameter :: PCRE_INFO_MINLENGTH           = 15
  integer, parameter :: PCRE_INFO_JIT                 = 16
  integer, parameter :: PCRE_INFO_JITSIZE             = 17
  integer, parameter :: PCRE_INFO_MAXLOOKBEHIND       = 18
  integer, parameter :: PCRE_INFO_FIRSTCHARACTER      = 19
  integer, parameter :: PCRE_INFO_FIRSTCHARACTERFLAGS = 20
  integer, parameter :: PCRE_INFO_REQUIREDCHAR        = 21
  integer, parameter :: PCRE_INFO_REQUIREDCHARFLAGS   = 22
  integer, parameter :: PCRE_INFO_MATCHLIMIT          = 23
  integer, parameter :: PCRE_INFO_RECURSIONLIMIT      = 24
  integer, parameter :: PCRE_INFO_MATCH_EMPTY         = 25

  type pcre_type
     type(c_ptr) :: regex
  end type pcre_type

  interface
     ! pcre *pcre_compile(const char *pattern, int options, const char **errptr, int *erroffset, const unsigned char *tableptr);
     function c_pcre_compile(pattern, options, errptr, erroffset, &
          tableptr) result(pcre) bind(C, name="pcre_compile")
       import
       type(c_ptr) :: pcre
       character(len=1,kind=c_char), intent(in) :: pattern
       integer(c_int), intent(in), value :: options
       type(c_ptr), intent(inout) :: errptr
       integer(c_int), intent(out) :: erroffset
       type(c_ptr), intent(in) :: tableptr
     end function c_pcre_compile

     ! int pcre_exec(const pcre*, const pcre_extra*, const char*,
     !               int, int, int, int*, int);
     function c_pcre_exec(code, extra, subject, length, startoffset, &
          options, ovector, ovecsize) &
          result(error) bind(C, name="pcre_exec")
       import
       type(c_ptr), value, intent(in) :: code
       type(c_ptr), value, intent(in) :: extra
       character(len=1,kind=c_char), intent(in) :: subject
       integer(c_int), value, intent(in) :: length
       integer(c_int), value, intent(in) :: startoffset
       integer(c_int), value, intent(in) :: options
       integer(c_int), value, intent(in) :: ovecsize
       integer(c_int), dimension(ovecsize), intent(out) :: ovector
       integer(c_int) :: error
     end function c_pcre_exec

     ! int pcre_fullinfo(const pcre*, const pcre_extra*, int, void*);
     function c_pcre_fullinfo(code, extra, what, where) &
          result(error) bind(C, name="pcre_fullinfo")
       import
       type(c_ptr), value, intent(in) :: code
       type(c_ptr), value, intent(in) :: extra
       integer(c_int), value, intent(in) :: what
       type(c_ptr), intent(out) :: where
       integer(c_int) :: error
     end function c_pcre_fullinfo

     ! subroutine pcre_free bind(C, name="pcre_free")
     ! end subroutine pcre_free

  end interface

contains

  function pcre_compile(pattern, options) result(regex)
    implicit none
    type(pcre_type) :: regex

    character(len=*), intent(in) :: pattern
    integer(c_int), intent(in) :: options

    character(len=len_trim(pattern)+1, kind=c_char) :: c_pattern
    type(c_ptr) :: c_pcre
    type(c_ptr) :: error
    integer(c_int) :: erroffset

    ! NULL-terminate pattern
    c_pattern = trim(pattern) // c_null_char

    c_pcre = c_pcre_compile(c_pattern, options, error, erroffset, &
         c_null_ptr)

    regex%regex = c_pcre

  end function pcre_compile

  function pcre_exec(regex, extra, subject, startoffset, &
       options, ovector) result(error)
    implicit none
    type(pcre_type), intent(in) :: regex
    type(c_ptr), intent(in) :: extra
    character(len=*), intent(in) :: subject
    integer, intent(in) :: startoffset
    integer, intent(in) :: options
    integer, dimension(:), intent(out) :: ovector
    integer :: error

    character(len=len_trim(subject)+1, kind=c_char) :: c_subject

    ! NULL-terminate subject
    c_subject = trim(subject) // c_null_char

    error = c_pcre_exec(regex%regex, extra, c_subject, &
         len_trim(subject)+1, startoffset, options, &
         ovector, size(ovector))

  end function pcre_exec

  function pcre_fullinfo(regex, extra, what, where) result(error)
    implicit none
    type(pcre_type), intent(in) :: regex
    type(c_ptr), intent(in) :: extra
    integer(c_int), intent(in) :: what
    type(c_ptr), intent(out) :: where
    integer(c_int) :: error

    error = c_pcre_fullinfo(regex%regex, extra, what, where)
  end function pcre_fullinfo

end module pcre_module
