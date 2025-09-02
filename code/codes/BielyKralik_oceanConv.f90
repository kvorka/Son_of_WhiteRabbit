program BielyKralik_oceanConv
  use ocean
  implicit none
  
  type(T_ocean) :: oceanmodel
  
  call oceanmodel%init_sub()
  
  do
    call oceanmodel%iter_sub()
  end do
  
  call oceanmodel%deallocate_sub()
  
end program BielyKralik_oceanConv
  