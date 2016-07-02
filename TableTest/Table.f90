program table
  implicit none

	character(*), PARAMETER :: EC = achar(27), ED = EC//'[00m', &
  UL = EC//'[21m', OL = EC//'[53m'

  character(10), dimension(:), allocatable :: names
  integer, dimension(:,:), allocatable :: predictions, tricks
  integer :: numPlayers, totalRounds, round, currentPlayer, &
          alloc_status, i, j, k, l

  write(*,'(a)',advance = "NO") "Enter the number of players"
  read*, numPlayers
  write(*,'(a,i1)'), "Number of player: ", numPlayers

  totalRounds = 60/numPlayers !there are 60 cards total
  round = 1

  allocate(names(numPlayers), predictions(totalRounds,numPlayers), &
          tricks(totalRounds,numPlayers), stat=alloc_status)
  if (alloc_status>0) then
    print*,"allocation failed. program ends."
    stop
  end if

  do i = 1, numPlayers
    write(*,'(a,i1,a)',advance = "NO"), "Enter player ",i," name:"
    read*,names(i)
    write(*,'(a,i1,a,a,5/)',advance = "NO"), "Player ",i," Name: ", names(i)
  end do

  do round = 1, totalRounds
    do i = 1, numPlayers
      print*, names(i)//" prediciton:"
      read*,predictions(round,i)
      write(*,'(a,a,a,i2,5/)',advance = "NO"), "Player ",names(i)," prediciton: ", predictions(round,i)
    end do

    do i = 1, numPlayers
      print*, names(i)//" tricks:"
      read*,tricks(round,i)
      write(*,'(a,a,a,i2,5/)',advance = "NO"), "Player ",names(i)," prediciton: ", tricks(round,i)
    end do

    call PrintTable()
  end do

contains
  subroutine PrintTable
    write(*, "(a)", advance='NO') UL//"|Round |"
    do k = 1, numPlayers
      write(*, "(a)", advance='NO') names(k)//"|"
    end do
    write(*, "(a)") ED

    do i = 1, round
      write(*, "(i6,a)", advance='NO') round," |"
      write(*, "(a)", advance='NO') names(k)//"|"
    end do
  end subroutine PrintTable

end program
