program BielyKralik_oceanConv
    use ocean
    implicit none
  
    type(T_ocean) :: oceanmodel
    
    !Inicializuj vypocet
    call oceanmodel%init_sub()
    
    !Casova slucka
    do
      call oceanmodel%iter_sub()
    end do
    
    !Cistenie
    call oceanmodel%deallocate_sub()
  
end program BielyKralik_oceanConv
  