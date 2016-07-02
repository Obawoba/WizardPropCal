module cards
  implicit none
  private
  public::card, getCard, trumpColor

  type :: card(color,value)
    character :: color
    integer :: value
  end type card

  interface operator (>)
    function takes(a,b)
      type(card),intent(in) :: a,b
      logical :: takes
      !implement card comparison here!
    end function
  end interface operator

  character :: trumpColor


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
  end function

  character function getCardColor(inputChar)
    character(3),intent(in) :: inputChar
    select case inputChar(1)
    case('r','g','b','y','n','z')
      getCardColor = inputChar(1)
    case default
      getCardColor = 'x'
  end function

end module cards
