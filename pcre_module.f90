module pcre_module

  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env

  type pcre_type
     type(c_ptr) :: regex
  end type pcre_type

  interface
     function c_pcre_compile(pattern, options, errptr, erroffset, &
          tableptr) result(pcre) bind(C, name="pcre_compile")
       import
       type(c_ptr) :: pcre
       type(c_ptr), intent(in) :: pattern
       integer(c_int), intent(in), value :: options
       type(c_ptr), intent(inout) :: errptr
       type(c_ptr), intent(out) :: erroffset
       type(c_ptr), intent(in) :: tableptr
     end function c_pcre_compile

     ! function pcre_exec() result() bind(C, name="pcre_exec")
     ! end function pcre_exec

     ! subroutine pcre_fullinfo bind(C, name="pcre_fullinfo")
     ! end subroutine pcre_fullinfo

     ! subroutine pcre_free bind(C, name="pcre_free")
     ! end subroutine pcre_free

  end interface

contains

  function pcre_compile(pattern, options) result(pcre)
    type(pcre_type) :: pcre

    character(len=*), intent(in) :: pattern
    integer(c_int), intent(in) :: options

    character(len=:), allocatable, target :: f_pattern
    type(c_ptr) :: c_pattern
    type(c_ptr) :: c_pcre
    type(c_ptr) :: error
    type(c_ptr) :: erroffset

    ! NULL-terminate pattern
    f_pattern = pattern // c_null_char
    ! Get C address of pattern
    c_pattern = c_loc(f_pattern)

    c_pcre = c_pcre_compile(c_pattern, options, error, erroffset, &
         c_null_ptr)

    pcre%regex = c_pcre
    ! call c_f_pointer(c_pcre, pcre%regex)

  end function pcre_compile
  
end module pcre_module
