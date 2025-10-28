submodule (physicalobject) spheroidal_matrix
  implicit none ; contains
  
  module procedure prepare_mat_mech_sub
    integer :: ij
    
    !$omp parallel do
    do ij = 1, this%jmax
      call this%mat%mech(ij)%fill_sub( this%mat_mech_fn( ij, this%cf   ), &
                                     & this%mat_mech_fn( ij, this%cf-1 )  )
    end do
    !$omp end parallel do
    
  end procedure prepare_mat_mech_sub
  
  module procedure mat_mech_fn
    integer        :: ir, is
    real(kind=dbl) :: facrr, facr, facj1, facj2, facpr1, facpr2, facvr1, facvr2
    
    allocate( matica(18,5*this%nd+2) )
      call zero_rarray_sub( 18*(5*this%nd+2), matica )
    
    facj1 = -sqrt( ( j   ) / ( 2*j+one ) )
    facj2 = +sqrt( ( j+1 ) / ( 2*j+one ) )
    
    ir = 1
      is = 1
        matica(10,is) = this%rad_grid%c(ir,-1)
        matica(15,is) = this%rad_grid%c(ir,+1)
        
        matica(10,is+1) = this%rad_grid%c(ir,-1)
        matica(15,is+1) = this%rad_grid%c(ir,+1)
        
    do ir = 1, this%nd
      is  = 5*(ir-1)+1
      
      facr   = 1 / this%rad_grid%r(ir)**2
      facrr  = 1 / this%rad_grid%rr(ir)**2
      
      facvr1 = - facj1 * ( j-1 ) / this%rad_grid%r(ir)
      facvr2 = + facj2 * ( j+2 ) / this%rad_grid%r(ir)
      
      if ( ir > 1 ) then
        facpr1 = +facj1 * ( j+1 ) / this%rad_grid%rr(ir)
        facpr2 = -facj2 * ( j   ) / this%rad_grid%rr(ir)
        
        matica( 2,is) = a * ( facj1 * this%rad_grid%dd(ir,-2)                                    )
        matica( 3,is) = a * ( facrr * this%rad_grid%dd(ir,-2)                                    )
        matica( 7,is) = a * ( facj1 * this%rad_grid%dd(ir,-1) + facpr1 * this%rad_grid%cc(ir,-1) )
        matica( 8,is) = a * ( facrr * this%rad_grid%dd(ir,-1)                                    )
        matica(10,is) = -1 / ( this%Pr * this%dt ) - a * this%hdiff_fn(j) * (j-1)*j * facrr
        matica(12,is) = a * ( facj1 * this%rad_grid%dd(ir,+1) + facpr1 * this%rad_grid%cc(ir,+1) )
        matica(13,is) = a * ( facrr * this%rad_grid%dd(ir,+1)                                    )
        matica(17,is) = a * ( facj1 * this%rad_grid%dd(ir,+2)                                    )
        matica(18,is) = a * ( facrr * this%rad_grid%dd(ir,+2)                                    )
        
        matica( 1,is+1) = a * ( facj2 * this%rad_grid%dd(ir,-2)                                    )
        matica( 3,is+1) = a * ( facrr * this%rad_grid%dd(ir,-2)                                    )
        matica( 6,is+1) = a * ( facj2 * this%rad_grid%dd(ir,-1) + facpr2 * this%rad_grid%cc(ir,-1) )
        matica( 8,is+1) = a * ( facrr * this%rad_grid%dd(ir,-1)                                    )
        matica(10,is+1) = -1 / ( this%Pr * this%dt ) - a * this%hdiff_fn(j) * (j+1)*(j+2) * facrr
        matica(11,is+1) = a * ( facj2 * this%rad_grid%dd(ir,+1) + facpr2 * this%rad_grid%cc(ir,+1) )
        matica(13,is+1) = a * ( facrr * this%rad_grid%dd(ir,+1)                                    )
        matica(16,is+1) = a * ( facj2 * this%rad_grid%dd(ir,+2)                                    )
        matica(18,is+1) = a * ( facrr * this%rad_grid%dd(ir,+2)                                    )
      end if
      
      matica( 8,is+2) = facvr1 * this%rad_grid%c(ir,-1)
      matica( 9,is+2) = facvr2 * this%rad_grid%c(ir,-1)
      matica(11,is+2) = facj1 * facr
      matica(12,is+2) = facj2 * facr
      matica(13,is+2) = facvr1 * this%rad_grid%c(ir,+1)
      matica(14,is+2) = facvr2 * this%rad_grid%c(ir,+1)
      
      matica( 2,is+3) = -this%rad_grid%d(ir,-2)
      matica( 7,is+3) = -this%rad_grid%d(ir,-1)
      matica(10,is+3) = facr
      matica(12,is+3) = -this%rad_grid%d(ir,+1)
      matica(17,is+3) = -this%rad_grid%d(ir,+2)
      
      matica( 2,is+4) = -this%rad_grid%d(ir,-2)
      matica( 7,is+4) = -this%rad_grid%d(ir,-1)
      matica(10,is+4) = facr
      matica(12,is+4) = -this%rad_grid%d(ir,+1)
      matica(17,is+4) = -this%rad_grid%d(ir,+2)
    end do
    
    ir = this%nd
      is = 5*ir+1
        matica( 5,is) = this%rad_grid%c(ir,-1)
        matica(10,is) = this%rad_grid%c(ir,+1)
        
        matica( 5,is+1) = this%rad_grid%c(ir,-1)
        matica(10,is+1) = this%rad_grid%c(ir,+1)
    
  end procedure mat_mech_fn
  
end submodule spheroidal_matrix