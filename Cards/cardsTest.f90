program cardsTest
  implicit none

  use cards

  type(card) :: testCard
  character(3) :: inputChar

  print*,'play a card!'
  read*,inputChar

  testCard = card(inputChar)
  select case testCard % color
    case ('r')
      print*, 'red'
    case ('g')
  print*, 'green'
    case ('b')
  print*, 'blue'
    case ('y')
      print*, 'yellow'
    case default
  end select

end program cardsTest
