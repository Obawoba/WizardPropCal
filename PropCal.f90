program PropCal
	implicit none

! Enable colors in terminal
! EC is the escape character
! EC is followed by '[$NUM$m]' where $NUM$ is a format specific Number
! EC//'[91mr'//EC//'[0m
	character(*), PARAMETER :: &
		EC = achar(27), ED = EC//'[0m', &
		RED = EC//'[91m', GREEN = EC//'[92m', YELLOW = EC//'[93m', BLUE = EC//'[96m'
	!
	character(5),DIMENSION(4) :: ColorCodes

	integer :: NumCards = 0, NumPlayers = 0, Position = 0
	character(3) :: TrumpCard				!format: eg. 'g9' 'b12' 'n'
	character(8) :: TrumpCardCC			!Added color coding(CC)

	!counters
	integer :: i = 0, j = 0, k = 0, l = 0

	!remeber all cards in game
	logical,DIMENSION(4,13) :: CardsInGame !Tag Array of all cards that haven't been played yet
	integer :: Wizards = 4, Fools = 4

!Initialize ColorCodes
ColorCodes(1)=RED
ColorCodes(2)=GREEN
ColorCodes(3)=BLUE
ColorCodes(4)=YELLOW

print*,ColorCodes(1)
print*,ColorCodes(2)
print*,ColorCodes(3)
print*,ColorCodes(4)
print*,ED

!Initialize Cards Array
	do i=1,13
		do j = 1,4
			CardsInGame(j,i) = .TRUE.
		end do
	end do

	if(PrintCards()) then
		print*,"hat geklappt"
	else
		print*,"wphle her nich nä?"
	end if

	print*, "This program calculates card propabilities"
	print*,

	!do while ((NumPlayers < 2) .and. (NumPlayers > 7))
		print*, "Number of Players (3 to 6)"
		read*, NumPlayers
		print*, NumPlayers
	!enddo

	print*, "Number of Cards (aka round)"
	read*, NumCards
	print*, "Number of Cards is: ",NumCards

	print*, "Trump card (eg. g9 b12 n)"
	print*, "Valid colors are: " &
					//RED//'r '//GREEN//'g '//BLUE//'b '//YELLOW//'y '//ED//'n'
	read*, TrumpCard
	TrumpCardCC = TrumpCardCodeChar(TrumpCard)
	print*, TrumpCardCC

contains
	character(8) function TrumpCardCodeChar(TrumpCard)
		implicit none
		character(3), INTENT(IN) :: TrumpCard
		character(6) :: TrumpColor
		TrumpColor = TrumpCard(:1)
		select case (TrumpColor)
			case('r')
				TrumpColor = RED//'r'
			case('g')
				TrumpColor = GREEN//'g'
			case('b')
				TrumpColor = BLUE//'b'
			case('y')
				TrumpColor = YELLOW//'y'
		end select
		TrumpCardCodeChar = TrumpColor//TrumpCard(2:)
	end function

	logical function PrintCards()
		implicit none
		character(5),DIMENSION(4) :: tags
		character(3) :: CardValue
		do i = 1,13
			do j = 1,4
				if(CardsInGame(j,i))then
					tags(j) = ColorCodes(j)
				else
					tags(j) = ED
				end if
			end do
			CardValue = itoc(i)
			print*,tags(1)//CardValue//tags(2)//CardValue//tags(3)//CardValue//tags(4)//CardValue//ED
		end do
		PrintCards = .TRUE.
	end function

	character(3) function itoc(input)
		integer, INTENT(IN) :: input
		character(12) :: tmp
		write(tmp,*) input
		tmp = adjustl(tmp)
		itoc = tmp(:3)
	end function
end program PropCal
