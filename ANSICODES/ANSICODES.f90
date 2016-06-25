program PropCal
	implicit none
	
	INTEGER :: i
	character(20) :: chr
	character(*), PARAMETER :: EC = achar(27), ED = EC//'[0m'
	
	do i = 0, 100, 1
		chr = str(i)
		print *, EC//'['//chr//'m '//chr//' Some Text'//ED
	end do

contains
	character(len=19) function str(k)
	!   "Convert an integer to string."
		integer, intent(in) :: k
		write (str, *) k
		str = adjustl(str)
	end function str
end program PropCal