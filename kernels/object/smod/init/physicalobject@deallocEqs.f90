submodule (physicalobject) deallocEqs
  implicit none; contains
  
  module procedure deallocEqs_sub
    integer :: ij
    
    if ( allocated( eqs_array ) ) then
      
      do ij = 0, this%jmax
        call eqs_array(ij)%deallocate_sub()
      end do
      
      deallocate( eqs_array )
      
    end if
    
  end procedure deallocEqs_sub
  
end submodule deallocEqs