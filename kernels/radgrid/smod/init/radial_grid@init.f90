submodule (radial_grid) init
  implicit none ; contains
  
  module procedure init_grid_sub
    integer        :: ir
    real(kind=qbl) :: pi_nr, cos_pi_nr, rdu2
    
    !! Number of main discretization points
    this%nd = nr
    
    !! Useful constants in quadruple precision
    pi_nr     = qpi / ( 2 * nr )
    cos_pi_nr = qone / ( 2 * cos( pi_nr ) )
    rdu2      = ( rd + ru ) / 2._qbl
    
    !! Prepare the main grid
    allocate( this%r(nr) )
      
      !$omp simd
      do ir = 1, nr
        this%r(ir) = real( rdu2 - cos( (2*ir-1) * pi_nr ) * cos_pi_nr, kind=dbl )
      end do
      
    !! Prepare the secondary grid with two ghost points
    allocate( this%rr(nr+1) )
      
      !$omp simd
      do ir = 1, nr+1 
        this%rr(ir) = real( rdu2 - cos( 2*(ir-1) * pi_nr ) * cos_pi_nr, kind=dbl )
      end do
      
    !! Save the volume of the shell for
    !! future use
    this%volume = 4 * pi * ( ru**3 - rd**3 ) / 3
    
  end procedure init_grid_sub
  
  module procedure deallocate_grid_sub
    
    deallocate( this%r  )
    deallocate( this%rr )
    
  end procedure deallocate_grid_sub
  
end submodule init