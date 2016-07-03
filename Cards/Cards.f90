module cards
  implicit none
  private
  public:: getCard, trumpColor, operator(>)

  type :: card
    character :: color
    integer :: value
  end type card

  character :: trumpColor

  interface operator (>)
    procedure cardTakesCard
  end interface operator (>)




contains
  function getCard(input) result(c)
    character(3), intent(in) :: input
    type(card) :: c
    c%value=getCardValue(input)
    c%color=getCardColor(input)
  end function getCard

  function getCardValue(input) result(val)
    character(3),INTENT(IN) :: input
    integer :: val

    if(input(3:)==' ') then !One digit!
      read(input(2:2),'(i4)') val
    else                    !two digits!
      read(input(2:3),'(i4)') val
    end if

    if(val>13.or.val<0)then
      val = -1
    end if
  end function getCardValue

  function getCardColor(input) result(color)
    character(3),intent(in) :: input
    character :: color
    select case(input(1))
    case('r','g','b','y','n','z')
      color = input(1)
    case default
      color = 'x'
    end select
  end function getCardColor

  function isTrump(c) result(r)
    type(card), intent(in) :: c
    logical :: r
    r=trumpColor==c%color
  end function isTrump

  function cardTakesCard(c1,c2) result(r)
    type(card), intent(in) :: c1,c2

    !error conditions
    if(c1%color=='x'.or.c2%color=='x') then
      print*,"Card had no color!"
    else if(c1%value==-1.or.c2%value==-1) then
      print*,"Card had no value!"

    !fools and wizards
    else if(c2%color=='z') then
      r = .false.
    else if(c1%color=='z') then
      r = .true.
    else if(c1%color=='n') then
      r = .false.
    else if(c2%color=='n') then
      r = .true.

    !both trump
    else if(isTrump(c1).and.isTrump(c2)) then
      r=c1%value>c2%value

    !neither trump
    else if(.not.(isTrump(c1).or.isTrump(c2)))
      if(c1%color==c2%color)then
        r=c1%value>c2%value
      else
        r=.false.
      end if

      !one trump


      !default
    else
      r = .false.
    end if
  end function

end module cards
