submodule (physicalobject) spheroidal_matrix
  implicit none ; contains
  
  module procedure mat_mech_fn
    integer        :: ir, is
    real(kind=dbl) :: j
    
    allocate( matica(23,6*this%nd+2) ); associate( grid => this%rad_grid )
    
    call zero_rarray_sub( 23*(6*this%nd+2), matica )
    
    j = i2r_fn(j_in)
    
    ir = 1
      is = 1
        select case (this%mechanic_bnd)
          case('frees')
            matica(14,is) = +sqrt((j+1)*(j-1)/(  (2*j+1)*(2*j-1)))
            matica(16,is) = -sqrt((3  )      /(2*(2*j-1)*(2*j+3)))
            matica(17,is) = -sqrt((j  )*(j+2)/(  (2*j+1)*(2*j+3)))
            
            matica(11,is+1) = +sqrt((j  )/(2*j+1)) * grid%c(ir,-1)
            matica(12,is+1) = -sqrt((j+1)/(2*j+1)) * grid%c(ir,-1)
            matica(17,is+1) = +sqrt((j  )/(2*j+1)) * grid%c(ir,+1)
            matica(18,is+1) = -sqrt((j+1)/(2*j+1)) * grid%c(ir,+1)
          
          case('noslp')
            matica(12,is) = grid%c(ir,-1)
            matica(18,is) = grid%c(ir,+1)
        
            matica(12,is+1) = grid%c(ir,-1)
            matica(18,is+1) = grid%c(ir,+1)   
        end select
  
    do ir = 1, this%nd
      is = 6*(ir-1)+1
      
        if (ir > 1) then
          matica( 2,is) = +a_in*sqrt((j-1)        /   (2*j-1)         )*(grid%dd(ir,-2)                                   )
          matica( 3,is) = -a_in*sqrt((j  )        /(3*(2*j+1)        ))*(grid%dd(ir,-2)                                   )
          matica( 4,is) = -a_in*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%dd(ir,-2)                                   )
          matica( 8,is) = +a_in*sqrt((j-1)        /   (2*j-1)         )*(grid%dd(ir,-1) - grid%cc(ir,-1)*(j-2)/grid%rr(ir))
          matica( 9,is) = -a_in*sqrt((j  )        /(3*(2*j+1)        ))*(grid%dd(ir,-1) + grid%cc(ir,-1)*(j+1)/grid%rr(ir))
          matica(10,is) = -a_in*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%dd(ir,-1) + grid%cc(ir,-1)*(j+1)/grid%rr(ir))
          matica(12,is) = -1 / ( this%Pr * this%dt )
          matica(14,is) = +a_in*sqrt((j-1)        /   (2*j-1)         )*(grid%dd(ir,+1) - grid%cc(ir,+1)*(j-2)/grid%rr(ir))
          matica(15,is) = -a_in*sqrt((j  )        /(3*(2*j+1)        ))*(grid%dd(ir,+1) + grid%cc(ir,+1)*(j+1)/grid%rr(ir))
          matica(16,is) = -a_in*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%dd(ir,+1) + grid%cc(ir,+1)*(j+1)/grid%rr(ir))
          matica(20,is) = +a_in*sqrt((j-1)        /   (2*j-1)         )*(grid%dd(ir,+2)                                   )
          matica(21,is) = -a_in*sqrt((j  )        /(3*(2*j+1)        ))*(grid%dd(ir,+2)                                   )
          matica(22,is) = -a_in*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%dd(ir,+2)                                   )
          
          matica( 2,is+1) = +a_in*sqrt((j+1)        /(3*(2*j+1)        ))*(grid%dd(ir,-2)                                   )
          matica( 3,is+1) = +a_in*sqrt((j  )*(2*j-1)/(6*(2*j+3)*(2*j+1)))*(grid%dd(ir,-2)                                   )
          matica( 4,is+1) = -a_in*sqrt((j+2)        /(2*j+3)            )*(grid%dd(ir,-2)                                   )
          matica( 8,is+1) = +a_in*sqrt((j+1)        /(3*(2*j+1)        ))*(grid%dd(ir,-1) - grid%cc(ir,-1)*(j  )/grid%rr(ir))
          matica( 9,is+1) = +a_in*sqrt((j  )*(2*j-1)/(6*(2*j+3)*(2*j+1)))*(grid%dd(ir,-1) - grid%cc(ir,-1)*(j  )/grid%rr(ir))
          matica(10,is+1) = -a_in*sqrt((j+2)        /(2*j+3)            )*(grid%dd(ir,-1) + grid%cc(ir,-1)*(j+3)/grid%rr(ir))
          matica(12,is+1) = -1 / ( this%Pr * this%dt )
          matica(14,is+1) = +a_in*sqrt((j+1)        /(3*(2*j+1)        ))*(grid%dd(ir,+1) - grid%cc(ir,+1)*(j  )/grid%rr(ir))
          matica(15,is+1) = +a_in*sqrt((j  )*(2*j-1)/(6*(2*j+3)*(2*j+1)))*(grid%dd(ir,+1) - grid%cc(ir,+1)*(j  )/grid%rr(ir))
          matica(16,is+1) = -a_in*sqrt((j+2)        /(2*j+3)            )*(grid%dd(ir,+1) + grid%cc(ir,+1)*(j+3)/grid%rr(ir))
          matica(20,is+1) = +a_in*sqrt((j+1)        /(3*(2*j+1)        ))*(grid%dd(ir,+2)                                   )
          matica(21,is+1) = +a_in*sqrt((j  )*(2*j-1)/(6*(2*j+3)*(2*j+1)))*(grid%dd(ir,+2)                                   )
          matica(22,is+1) = -a_in*sqrt((j+2)        /(2*j+3)            )*(grid%dd(ir,+2)                                   )
        end if
        
        matica( 4,is+2) = +sqrt((j  )/(2*j+1))*(grid%d(ir,-2)                                 )
        matica( 5,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,-2)                                 )
        matica(10,is+2) = +sqrt((j  )/(2*j+1))*(grid%d(ir,-1) - grid%c(ir,-1)*(j-1)/grid%r(ir))
        matica(11,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,-1) + grid%c(ir,-1)*(j+2)/grid%r(ir))
        matica(16,is+2) = +sqrt((j  )/(2*j+1))*(grid%d(ir,+1) - grid%c(ir,+1)*(j-1)/grid%r(ir))
        matica(17,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,+1) + grid%c(ir,+1)*(j+2)/grid%r(ir))
        matica(22,is+2) = +sqrt((j  )/(2*j+1))*(grid%d(ir,+2)                                 )
        matica(23,is+2) = -sqrt((j+1)/(2*j+1))*(grid%d(ir,+2)                                 )
        
        matica( 3,is+3) = -2*sqrt((j-1)/(2*j-1))*(grid%d(ir,-2)                             )
        matica( 9,is+3) = -2*sqrt((j-1)/(2*j-1))*(grid%d(ir,-1) + grid%c(ir,-1)*j/grid%r(ir))
        matica(11,is+3) = one
        matica(15,is+3) = -2*sqrt((j-1)/(2*j-1))*(grid%d(ir,+1) + grid%c(ir,+1)*j/grid%r(ir))
        matica(21,is+3) = -2*sqrt((j-1)/(2*j-1))*(grid%d(ir,+2)                             )
        
        matica( 2,is+4) = +2*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%d(ir,-2)                               )
        matica( 3,is+4) = -2*sqrt((j  )*(2*j-1)/(6*(2*j+1)*(2*j+3)))*(grid%d(ir,-2)                               )
        matica( 8,is+4) = +2*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%d(ir,-1)-grid%c(ir,-1)*(j-1)/grid%r(ir))
        matica( 9,is+4) = -2*sqrt((j  )*(2*j-1)/(6*(2*j+1)*(2*j+3)))*(grid%d(ir,-1)+grid%c(ir,-1)*(j+2)/grid%r(ir))
        matica(12,is+4) = one
        matica(14,is+4) = +2*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%d(ir,+1)-grid%c(ir,+1)*(j-1)/grid%r(ir))
        matica(15,is+4) = -2*sqrt((j  )*(2*j-1)/(6*(2*j+1)*(2*j+3)))*(grid%d(ir,+1)+grid%c(ir,+1)*(j+2)/grid%r(ir))
        matica(20,is+4) = +2*sqrt((j+1)*(2*j+3)/(6*(2*j-1)*(2*j+1)))*(grid%d(ir,+2)                               )
        matica(21,is+4) = -2*sqrt((j  )*(2*j-1)/(6*(2*j+1)*(2*j+3)))*(grid%d(ir,+2)                               )
        
        matica( 2,is+5) = +2*sqrt((j+2)/(2*j+3))*(grid%d(ir,-2)                                 )
        matica( 8,is+5) = +2*sqrt((j+2)/(2*j+3))*(grid%d(ir,-1) - grid%c(ir,-1)*(j+1)/grid%r(ir))
        matica(12,is+5) = one
        matica(14,is+5) = +2*sqrt((j+2)/(2*j+3))*(grid%d(ir,+1) - grid%c(ir,+1)*(j+1)/grid%r(ir))
        matica(20,is+5) = +2*sqrt((j+2)/(2*j+3))*(grid%d(ir,+2)                                 )
    end do
    
    ir = this%nd
      is = 6*this%nd+1
        select case (this%mechanic_bnd)
          case('frees')
            matica( 8,is) = +sqrt((j+1)*(j-1)/(  (2*j+1)*(2*j-1)))
            matica(10,is) = -sqrt((3  )      /(2*(2*j-1)*(2*j+3)))
            matica(11,is) = -sqrt((j  )*(j+2)/(  (2*j+1)*(2*j+3)))
            
            matica( 5,is+1) = +sqrt((j  )/(2*j+1)) * grid%c(ir,-1)
            matica( 6,is+1) = -sqrt((j+1)/(2*j+1)) * grid%c(ir,-1)
            matica(11,is+1) = +sqrt((j  )/(2*j+1)) * grid%c(ir,+1)
            matica(12,is+1) = -sqrt((j+1)/(2*j+1)) * grid%c(ir,+1)
            
          case('noslp')
            matica( 6,is) = grid%c(ir,-1)
            matica(12,is) = grid%c(ir,+1)
            
            matica( 6,is+1) = grid%c(ir,-1)
            matica(12,is+1) = grid%c(ir,+1)
        end select
    
    end associate
    
  end procedure mat_mech_fn
  
end submodule spheroidal_matrix