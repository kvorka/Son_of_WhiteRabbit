submodule (physicalobject) diffusion
  implicit none ; contains
  
  module procedure hdiff_fn
    integer :: jdiff
    
    select case ( this%diffusion_type )
      case ('isotr')
        hdiff_fn = one
      
      case ('mitgc')
        hdiff_fn = 10._dbl
      
      case ('hyper')
        jdiff = 7 * this%jmax / 10
        
        if ( j < jdiff ) then
          hdiff_fn = one
        else
          hdiff_fn = 1000._dbl**(  i2r_fn(j-jdiff) / i2r_fn(this%jmax-jdiff) )
        end if
      
      end select
      
  end procedure hdiff_fn
  
end submodule diffusion