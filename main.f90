program main
  use IniFile
  implicit none  
  ! assign_ functions only takes array arguments
  ! tmp provided as a temporary 1 element array
  ! tmp_vec provided as a temporaray allocatable array - must be deallocated after each assignement !!!

  real(kind=8), allocatable, dimension(:) :: tmp_real_vec
  real(kind=8) :: tmp_real

  real(kind=8) :: real1

  integer, allocatable, dimension(:) :: tmp_int_vec
  integer :: tmp_int

  integer :: int1

  character(len=100), allocatable, dimension(:) :: tmp_char_vec
  character(len=20) :: tmp_char

  character(len=20) :: str1
  character(len=7), allocatable, dimension(:) :: array_str

  call read_input('input.ini')
  call assign_real(real1,tmp_real_vec,'real1');deallocate(tmp_real_vec)
  call assign_str(str1,tmp_char_vec,'str1');deallocate(tmp_char_vec)
  call assign_integer(int1,tmp_int_vec,'int1');deallocate(tmp_int_vec)
  call assign_str(tmp_char,array_str,'array_str')

  deallocate(input)

  write(*,'(a,a)') " some string     ", str1
  write(*,'(a,f10.8)') " some float real ", real1
  write(*,'(a,i10)') " some integer ", int1
  write(*,*) "a bunch of strings    ", array_str

end program main