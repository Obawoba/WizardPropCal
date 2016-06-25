program PropCal
	implicit none

! Enable colors in terminal
! EC is the escape character
! EC is followed by '[$NUM$m]' where $NUM$ is a format specific Number
!
	character(*), PARAMETER :: &
		EC = achar(27), ED = EC//'[0m', &
		RED = EC//'[91m', GREEN = EC//'[92m', YELLOW = EC//'[93m', BLUE = EC//'[96m'

	integer :: NumCards, NumPlayers, Position
	character(1) :: TrumpColor


	print*, "This program calculates card propabilities"
	print*,

	do while (NumPlayers < 2 .and. NumPlayers > 7)
		print*, "Number of Players (3 to 6)"
		read*, NumPlayers
		print*, NumPlayers
	enddo

	print*, "Number of Cards (aka round)"
	read*, NumCards
	print*, "Number of Cards is: ",NumCards

	print*, "Color of trump (CANT STUMP THE TRUMP)"
	print*, "Valid colors are: " &
		//RED//'r '//GREEN//'g '//BLUE//'b '//YELLOW//'y '//ED//'n'
	read*, TrumpColor
	print*, TrumpColorCodeChar(TrumpColor)

contains
	character(6) function TrumpColorCodeChar(TrumpColor)
		implicit none
		character, INTENT(IN) :: TrumpColor
		select case (TrumpColor)
			case('r')
				TrumpColorCodeChar = RED//'r'
			case('g')
				TrumpColorCodeChar = GREEN//'g'
			case('b')
				TrumpColorCodeChar = BLUE//'b'
			case('y')
				TrumpColorCodeChar = YELLOW//'y'
		end select
	end function
end program PropCal
