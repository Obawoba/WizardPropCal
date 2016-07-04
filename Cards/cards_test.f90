program cardsTest
  use cards
  implicit none

  type(card) :: highestCard, someCard
  character(3) :: inputChar

  trumpColor = 'r'

  print*,'play a card!'
  read*,inputChar
  highestCard = getCard(inputchar)
  print*, 'you played ',&
    highestCard%color,highestCard%value

  print*, 'play another card!'
  read*,inputChar
  someCard = getCard(inputchar)
  if(someCard>highestCard)then
    print*, 'you beat that card!'
  else
    print*, 'that card was bad!'
  end if
end program cardsTest
