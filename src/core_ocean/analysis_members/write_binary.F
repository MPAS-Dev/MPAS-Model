program hello
  implicit none
  integer, parameter :: RKIND  = selected_real_kind(12)
  integer :: my_proc_id
  real (kind=RKIND) :: r, minT, maxT
  character(len=6) :: my_proc_id_str
  character(len=10) :: time_stamp
  character(len=50) :: file_name
  character(len=20) :: var
  real (kind=RKIND), dimension(33) :: data
  integer (kind=2), dimension(33) :: data_int
  integer :: i

  write(*,*) 'Hello world!'
  r = 1._RKIND / 3._RKIND

  minT = -2
  maxT = 30
  do i = 1,33
     data(i) = i-3
     data_int(i) = (data(i) - minT)/(maxT - minT) * 65535
  enddo
  my_proc_id = 23
  var = 'temperature'
  write (my_proc_id_str,'(I6.6)' ) my_proc_id
  print *, '1 my_proc_id_str',my_proc_id_str
  print *, '2 my_proc_id_str',adjustl(my_proc_id_str)
  time_stamp = '0056-12-07' 
  file_name = 'mpaso.'//trim(var)//'.'//time_stamp//'.p'//my_proc_id_str//'.dat'
  open(unit=10, file=file_name, status='replace', form='unformatted')
  write (10) data_int
  close(unit=10)

end program hello
