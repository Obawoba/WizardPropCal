program PropCal
  	implicit none

  ! Enable colors in terminal
  ! EC is the escape character (ASCII 27)
  ! EC is followed by '[$NUM$m]' where $NUM$ is a format specific Number
  ! EC//'[91mr'//EC//'[0m <= is a red r and disables color afterwards
  	character(*), PARAMETER :: &
    EC = achar(27), ED = EC//'[00m', &
      RED = EC//'[91m', GREEN = EC//'[92m',&
      YELLOW = EC//'[93m', BLUE = EC//'[96m'
    ! Array that contains all these color codes
    character(5),DIMENSION(4) :: ColorCodes

  	integer :: NumCards = 0, NumPlayers = 0, Position = 0, &
    CurrentCardColor, CurrentCardValue
  	character(3) :: TrumpCard , CurrentCard				!format: eg. 'g9' 'b12' 'n'
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



  !print*,ColorCodes(1)
  !print*,ColorCodes(2)
  !print*,ColorCodes(3)
  !print*,ColorCodes(4)
  !print*,ED

  !Initialize Cards Array
  	do i=1,13
  		do j = 1,4
  			CardsInGame(j,i) = .TRUE.
  		end do
  	end do

  10 continue
    call PrintCards()
  		print*, "Play a Card!"
  		read*, CurrentCard
  		CurrentCardColor = getCardColorNumber(CurrentCard)

  		if(CurrentCardColor==6) then !wizard
  			if(Wizards>0)then
  				Wizards = Wizards - 1
          print*, "It's a wizard!"
  			else
          print*, "Can't play that card!"
  			end if
        Go to 10

  		else if(CurrentCardColor == 5) then !fool
  			if(Fools>0)then
  				Fools = Fools - 1
          print*, "It's a fool!"
  			else
  				print*, "Can't play that card!"
  			end if
        Go to 10

  		else if(CurrentCardColor>0) then!normal card
        CurrentCardValue = getCardValue(CurrentCard)
        if(CurrentCardValue > 0) then
          if(CardsInGame(CurrentCardColor,CurrentCardValue)) then
            CardsInGame(CurrentCardColor,CurrentCardValue) = .FALSE.
            print*,"That was "//ColorCodes(CurrentCardColor)//CurrentCard//ED
          else
            print*,"This Card was already played"
          end if
          Go to 10
        else
          print*, "Something is odd"
        end if
      end if
  	!end of continue


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

  contains
  	subroutine PrintCards()
  		character(5),DIMENSION(4) :: Tags !ColorTags (colored if card not yet played)
  		character(2) :: CardValue					!Char that contains card number (1 to 13)

  		!print fools
  		print*, ' '//repeat("N  ", Fools)				!prints  "N " repeated fools times

  		!print the 1-13 cards
  		do i = 1,13												!for each card number
  			do j = 1,4											!for each color
  				if(CardsInGame(j,i))then
  					Tags(j) = ColorCodes(j)
  				else
  					Tags(j) = ED
  				end if
  			end do
  			CardValue = itoc(i)
  			print*,Tags(1)//CardValue//' '//Tags(2)//CardValue//' '// &
  							Tags(3)//CardValue//' '//Tags(4)//CardValue//' '//ED
  		end do

  		!print Wizards
  		print*, ' '//repeat("Z  ", Wizards)				!prints  "N " repeated fools times
  	end subroutine

  	character(5) function getCardColorCode(Card)
  		implicit none
  		character(3), INTENT(IN) :: Card
  		character :: Color

  		Color = Card(:1)
  		select case (Color)
  			case('r')
  				getCardColorCode = RED
  			case('g')
  				getCardColorCode = GREEN
  			case('b')
  				getCardColorCode = BLUE
  			case('y')
  				getCardColorCode = YELLOW
  		end select
  	end function

  	integer function getCardColorNumber(Card)
  		implicit none
  		character(3), INTENT(IN) :: Card
  		character :: Color

  		Color = Card(:1)
  		select case (Color)
  			case('r')
  				getCardColorNumber = 1
  			case('g')
  				getCardColorNumber = 2
  			case('b')
  				getCardColorNumber = 3
  			case('y')
  				getCardColorNumber = 4
  			case('n')
  				getCardColorNumber = 5
  			case('z')
  				getCardColorNumber = 6
  		end select
  	end function

    !return the card number 1 to 13
    integer function getCardValue(Card)
      character(3),INTENT(IN) :: Card
      if(Card(3:)==' ') then !One digit!
        read(Card(2:2),'(i4)') getCardValue
      else
        read(Card(2:3),'(i4)') getCardValue
      end if
    end function

    !obsolete
  	integer function ctoi(input)
  		character(*), INTENT(IN) :: input
  		character(2) :: tmp
  	end function

  	character(2) function itoc(input)
  		integer, INTENT(IN) :: input
  		character(12) :: tmp
  		write(tmp,*) input
  		itoc = tmp(11:)
  	end function
  end program PropCal
