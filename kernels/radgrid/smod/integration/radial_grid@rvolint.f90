submodule (radial_grid) rvolint
  implicit none; contains
  
  module procedure intV_fn
    integer        :: ir
    real(kind=dbl) :: cr11, cr12, cr21, cr22, dr
    
    !! Set the integral to zero
    intV = czero
    
    !! Check, whether we are on the main or secondary grid and start
    !! the integration procedure. The data are always interpolated
    !! into the middle of the main radial cells.
    if ( size(field) == this%nd+1 ) then
      
      !$omp parallel do private (dr,cr11,cr12,cr21,cr22) reduction (+:intV)
      do ir = 2, this%nd
        dr = this%r(ir) - this%r(ir-1)
        
        cr11 = this%c(ir-1,-1) * this%rr(ir-1)**2
        cr12 = this%c(ir-1,+1) * this%rr(ir  )**2
        cr21 = this%c(ir  ,-1) * this%rr(ir  )**2
        cr22 = this%c(ir  ,+1) * this%rr(ir+1)**2
        
        intV = intV + dr * ( cr11 * field(ir-1) + cr12 * field(ir  ) + &
                           & cr21 * field(ir  ) + cr22 * field(ir+1)   )
      end do
      !$omp end parallel do
      
    else
      
      !$omp parallel do private (dr,cr11,cr12) reduction (+:intV)
      do ir = 2, this%nd
        dr = this%r(ir) - this%r(ir-1)
        
        cr11 = this%r(ir-1)**2
        cr12 = this%r(ir  )**2
        
        intV = intV + dr * ( cr11 * field(ir-1) + cr12 * field(ir) )
      end do
      !$omp end parallel do
      
    end if
    
    !! Account for avereging into the middle of the cells
    intV = intV / 2
    
  end procedure intV_fn
  
end submodule rvolint