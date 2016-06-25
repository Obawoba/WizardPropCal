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
	character :: TrumpColor


	print*, "This program calculates card propabilities"
	print*,

	print*, "Number of Players"
	read*, NumPlayers
	print*, NumPlayers

	print*, "Number of Cards (aka round)"
	read*, NumCards
	print*, "Number of Cards is: " NumCards

contains

end program PropCal
