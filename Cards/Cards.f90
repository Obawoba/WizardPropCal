module cards
  implicit none

  type :: card(color,value)
    character :: color
    integer :: value
  end type card


  print*,'play a card!'
  read*,inputChar

  testCard = card(inputChar)
  select case testCard % color
    case ('r')

    case ('g')

    case ('b')

    case ('y')
  end select
contains
  type(card) function card(inputChar)
    character, intent(in) :: inputChar

  end function card

end module cards
