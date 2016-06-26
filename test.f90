program test
    implicit none
    integer :: tmp
    character(*), PARAMETER :: chr="myrest"
    print*,chr(1:1)
10  continue
    print*, "Enter any number"
    read*, tmp
    print*, tmp
    if(tmp>10) then
      goto 10
    end if

    tmp = ParseCardNumber(chr)
    print*, tmp
contains
    integer function ParseCardNumber(Input)
      character(2),INTENT(IN) :: Input
      if(Input(2:)==' ') then !One digit!
        read(Input(:1),'(i4)') ParseCardNumber
      else
        read(Input(:2),'(i4)') ParseCardNumber
      end if
    end function
end program test
