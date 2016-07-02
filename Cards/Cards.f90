module cardType
  type :: card
    character :: color
    integer :: value
  end type card
end module cardType

module cards
  use cardType
  implicit none
  !private
  !public:: getCard, trumpColor

  character :: trumpColor

  interface operator (>)
    logical function takes(a,b)
      type(card),intent(in) :: a,b

      !implement card comparison here!
    end function
  end interface operator (>)




contains
  type(card) function getCard(inputChar)
    character(3), intent(in) :: inputChar
    type(card) :: tmpcard
    tmpcard%value=getCardValue(inputChar)
    tmpcard%color=getCardColor(inputChar)
  end function getCard

  integer function getCardValue(inputChar)
    character(3),INTENT(IN) :: inputChar
    if(inputChar(3:)==' ') then !One digit!
      read(inputChar(2:2),'(i4)') getCardValue
    else                        !two digits!
      read(inputChar(2:3),'(i4)') getCardValue
    end if
  end function getCardValue

  character function getCardColor(inputChar)
    character(3),intent(in) :: inputChar
    select case(inputChar(1))
    case('r','g','b','y','n','z')
      getCardColor = inputChar(1)
    case default
      getCardColor = 'x'
    end select
  end function getCardColor

end module cards
