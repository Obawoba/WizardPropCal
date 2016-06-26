program PropCalv2
  implicit none




contains
  !Parse an input char of type 'r13' 'g9' 'n'
  !To integer i such that i mod 15 is the card value (0 is fool, 14 is wizard)
  integer function ParseCharToCard(Input)
    character(3),INTENT(IN) :: Input
    character(2) :: CharValue !value of card, this is Input(2:)
    character :: Color        !color of card, this is Input(:1)
    integer :: CardValue = 0      !Value of card as integer from 0 to 60
    CharValue = Input(2:)
    Color = Input(:1)

    if(Color=='r'.OR.Color=='g'.OR.Color=='b'.OR.Color=='y')then
      select case(color)
        case('r')
          CardValue = CardValue + 0
        case('g')
          CardValue = CardValue + 15

    end if
  end function

  integer function ParseCardNumber(Input) !read '0 ' to '15'
    character(2),INTENT(IN) :: Input
    if(Input(2:)==' ') then !One digit!
      read(Input(:1),'(i4)') ParseCardNumber
    else                    !Two digit!
      read(Input(:2),'(i4)') ParseCardNumber
    end if
  end function
end program PropCalv2
