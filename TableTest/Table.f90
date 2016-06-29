program table
  implicit none

	character(*), PARAMETER :: EC = achar(27), ED = EC//'[00m', &
  UL = EC//'[21m', OL = EC//'[53m'

  print*, "This should print a Table!"

  print*, UL//"This| IS | a  |test"//ED
  print*, "   1|   2|   3|   4"
  print*, "   1|   2|   3|   4"
  print*, "   1|   2|   3|   4"
  print*, "   1|   2|   3|   4"
  print*, "   1|   2|   3|   4"
  print*, OL//"yo  |this|work|out!"//ED
contains

end program
