program logical_plus_logical
  implicit none
  character(3) :: chr
  integer :: int
  chr='n  '; int=0
  read(chr(2:),'(i4)') int
  print*, chr//" becomes ", int
end program logical_plus_logical
