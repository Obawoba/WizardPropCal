program PropCal
	implicit none

	INTEGER :: i
	character(2) :: chr
	character(*), PARAMETER :: EC = achar(27), ED = EC//'[00m'

	do i = 0, 100, 1
		chr = itoc(i)
		print *, EC//'['//chr//'m '//chr//' Some Text'//ED
	end do

contains
	character(2) function itoc(input)
		integer, INTENT(IN) :: input
		character(12) :: tmp
		write(tmp,*) input
		itoc = tmp(11:)
	end function
end program PropCal
