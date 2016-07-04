program logical_plus_logical
  implicit none
  character(3) :: chr
  chr = 'x'

  print*, test(chr)

contains
  function test(in) result(out)
    character(3), intent(in) :: in
    character :: out
    out = in(1:1)
  end function
end program logical_plus_logical
