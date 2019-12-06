module pcre_module

  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env

  use pcre_constants

  implicit none

  type pcre_type
     type(c_ptr) :: regex
  end type pcre_type

  interface
     ! pcre *pcre_compile(const char *pattern, int options,
     !                    const char **errptr, int *erroffset,
     !                    const unsigned char *tableptr);
     function c_pcre_compile(pattern, options, errptr, erroffset, &
          tableptr) result(pcre) bind(C, name="pcre_compile")
       import
       type(c_ptr) :: pcre
       character(len=1, kind=c_char), intent(in) :: pattern(*)
       integer(c_int), intent(in), value :: options
       type(c_ptr), intent(inout) :: errptr
       integer(c_int), intent(out) :: erroffset
       type(c_ptr), value, intent(in) :: tableptr
     end function c_pcre_compile

     ! int pcre_exec(const pcre*, const pcre_extra*, const char*,
     !               int, int, int, int*, int);
     function c_pcre_exec(code, extra, subject, length, startoffset, &
          options, ovector, ovecsize) &
          result(error) bind(C, name="pcre_exec")
       import
       type(c_ptr), value, intent(in) :: code
       type(c_ptr), value, intent(in) :: extra
       character(len=1,kind=c_char), intent(in) :: subject(*)
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

     subroutine c_pcre_free(code) bind(C, name="pcre_free")
       import
       type(c_ptr) :: code
     end subroutine c_pcre_free

  end interface

contains

  function pcre_compile(pattern, options) result(regex)
    implicit none
    type(pcre_type) :: regex

    character(len=*), intent(in) :: pattern
    integer(c_int), intent(in) :: options

    character(len=len(pattern)+1, kind=c_char) :: c_pattern
    type(c_ptr) :: c_pcre
    type(c_ptr) :: error
    integer(c_int) :: erroffset

    ! NULL-terminate pattern
    c_pattern = pattern // c_null_char

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

    character(len=len(subject)+1, kind=c_char) :: c_subject

    ! NULL-terminate subject
    c_subject = subject // c_null_char

    error = c_pcre_exec(regex%regex, extra, c_subject, &
         len(subject), startoffset, options, &
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

  subroutine pcre_free(regex)
    implicit none
    type(pcre_type) :: regex

    call c_pcre_free(regex%regex)
  end subroutine pcre_free

end module pcre_module
