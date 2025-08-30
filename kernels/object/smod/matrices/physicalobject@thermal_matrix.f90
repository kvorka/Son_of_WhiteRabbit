submodule (physicalobject) thermal_matrix
  implicit none ; contains
  
  module procedure mat_temp_fn
    integer        :: ir, is
    real(kind=dbl) :: j, dT_dr

    allocate(matica(11,3*this%nd+1) ); associate( grid => this%rad_grid )
    
    call zero_rarray_sub( 11*(3*this%nd+1), matica )
    
    j = i2r_fn(j_in)
    
    ir = 1
      is = 1
        select case (this%thermal_bnd)
          case('fluxd')
            matica(7,is) = +sqrt((j  )/(2*j+1))
            matica(8,is) = -sqrt((j+1)/(2*j+1))
            
          case('basic')
            matica(6,is) = grid%c(ir,-1)
            matica(9,is) = grid%c(ir,+1)
        end select
    
    do ir = 1, this%nd
      is = 3*(ir-1)+1
      
        if (ir > 1) then
          matica( 1,is) = +a_in*sqrt((j  )/(2*j+1))*(grid%dd(ir,-2)                                   )
          matica( 2,is) = -a_in*sqrt((j+1)/(2*j+1))*(grid%dd(ir,-2)                                   )
          matica( 4,is) = +a_in*sqrt((j  )/(2*j+1))*(grid%dd(ir,-1) - grid%cc(ir,-1)*(j-1)/grid%rr(ir))
          matica( 5,is) = -a_in*sqrt((j+1)/(2*j+1))*(grid%dd(ir,-1) + grid%cc(ir,-1)*(j+2)/grid%rr(ir))
          matica( 6,is) = 1 / this%dt
          matica( 7,is) = +a_in*sqrt((j  )/(2*j+1))*(grid%dd(ir,+1) - grid%cc(ir,+1)*(j-1)/grid%rr(ir))
          matica( 8,is) = -a_in*sqrt((j+1)/(2*j+1))*(grid%dd(ir,+1) + grid%cc(ir,+1)*(j+2)/grid%rr(ir))
          matica(10,is) = +a_in*sqrt((j  )/(2*j+1))*(grid%dd(ir,+2)                                   )
          matica(11,is) = -a_in*sqrt((j+1)/(2*j+1))*(grid%dd(ir,+2)                                   )
        end if
        
        matica( 2,is+1) = +sqrt((j  )/(2*j+1))*(grid%d(ir,-2)                                 )
        matica( 5,is+1) = +sqrt((j  )/(2*j+1))*(grid%d(ir,-1) + grid%c(ir,-1)*(j+1)/grid%r(ir))
        matica( 6,is+1) = one
        matica( 8,is+1) = +sqrt((j  )/(2*j+1))*(grid%d(ir,+1) + grid%c(ir,+1)*(j+1)/grid%r(ir))
        matica(11,is+1) = +sqrt((j  )/(2*j+1))*(grid%d(ir,+2)                                 )
        
        matica( 1,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,-2)                                 )
        matica( 4,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,-1) - grid%c(ir,-1)*(j  )/grid%r(ir))
        matica( 6,is+2) = one
        matica( 7,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,+1) - grid%c(ir,+1)*(j  )/grid%r(ir))
        matica(10,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,+2)                                 )
    end do
    
    ir = this%nd
      is = 3*ir+1
        select case (this%thermal_bnd)
          case('basic', 'fluxd')
            matica(3,is) = grid%c(ir,-1)
            matica(6,is) = grid%c(ir,+1)
        end select
    
    end associate
    
  end procedure mat_temp_fn
  
end submodule thermal_matrix