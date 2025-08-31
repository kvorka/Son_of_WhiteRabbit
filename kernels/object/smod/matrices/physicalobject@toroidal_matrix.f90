submodule (physicalobject) toroidal_visc_matrix
  implicit none ; contains
  
  module procedure prepare_mat_torr_sub
    integer :: ij
    
    !$omp parallel do
    do ij = 1, this%jmax
      call this%mat%torr(ij)%fill_sub( this%mat_torr_fn(j_in=ij, a_in=this%cf  ), &
                                     & this%mat_torr_fn(j_in=ij, a_in=this%cf-1)  )
    end do
    !$omp end parallel do
    
  end procedure prepare_mat_torr_sub
  
  module procedure mat_torr_fn
    integer        :: ir, is
    real(kind=dbl) :: j

    allocate( matica(11, 3*this%nd+1) ); associate( grid => this%rad_grid )
    
    call zero_rarray_sub( 11*(3*this%nd+1), matica )
    
    j = i2r_fn(j_in)
    
    ir = 1
      is = 1
        select case (this%mechanic_bnd)
          case('frees')
            matica(7,is) = +sqrt((j-1)/(2*(2*j+1)))
            matica(8,is) = -sqrt((j+2)/(2*(2*j+1)))
          
          case('noslp')
            matica(6,is) = grid%c(ir,-1)
            matica(9,is) = grid%c(ir,+1)
        end select
  
    do ir = 1, this%nd
      is = 3*(ir-1)+1
        
        if (ir > 1) then
          matica( 2,is) = -a_in*sqrt((j+2)/(2*(2*j+1)))*(grid%dd(ir,-2)                                   )
          matica( 1,is) = +a_in*sqrt((j-1)/(2*(2*j+1)))*(grid%dd(ir,-2)                                   )
          matica( 4,is) = +a_in*sqrt((j-1)/(2*(2*j+1)))*(grid%dd(ir,-1) - grid%cc(ir,-1)*(j-1)/grid%rr(ir))
          matica( 5,is) = -a_in*sqrt((j+2)/(2*(2*j+1)))*(grid%dd(ir,-1) + grid%cc(ir,-1)*(j+2)/grid%rr(ir))
          matica( 6,is) = -1 / ( this%Pr * this%dt )
          matica( 7,is) = +a_in*sqrt((j-1)/(2*(2*j+1)))*(grid%dd(ir,+1) - grid%cc(ir,+1)*(j-1)/grid%rr(ir))
          matica( 8,is) = -a_in*sqrt((j+2)/(2*(2*j+1)))*(grid%dd(ir,+1) + grid%cc(ir,+1)*(j+2)/grid%rr(ir))
          matica(10,is) = +a_in*sqrt((j-1)/(2*(2*j+1)))*(grid%dd(ir,+2)                                   )
          matica(11,is) = -a_in*sqrt((j+2)/(2*(2*j+1)))*(grid%dd(ir,+2)                                   )
        end if
        
        matica( 2,is+1) = -2*sqrt((j-1)/(2*(2*j+1)))*(grid%d(ir,-2)                                 )
        matica( 5,is+1) = -2*sqrt((j-1)/(2*(2*j+1)))*(grid%d(ir,-1) + grid%c(ir,-1)*(j+1)/grid%r(ir))
        matica( 6,is+1) = one
        matica( 8,is+1) = -2*sqrt((j-1)/(2*(2*j+1)))*(grid%d(ir,+1) + grid%c(ir,+1)*(j+1)/grid%r(ir))
        matica(11,is+1) = -2*sqrt((j-1)/(2*(2*j+1)))*(grid%d(ir,+2)                                 )
        
        matica( 1,is+2) = +2*sqrt((j+2)/(2*(2*j+1)))*(grid%d(ir,-2)                                 )
        matica( 4,is+2) = +2*sqrt((j+2)/(2*(2*j+1)))*(grid%d(ir,-1) - grid%c(ir,-1)*(j  )/grid%r(ir))
        matica( 6,is+2) = one
        matica( 7,is+2) = +2*sqrt((j+2)/(2*(2*j+1)))*(grid%d(ir,+1) - grid%c(ir,+1)*(j  )/grid%r(ir))
        matica(10,is+2) = +2*sqrt((j+2)/(2*(2*j+1)))*(grid%d(ir,+2)                                 )
    end do
    
    ir = this%nd
      is = 3*this%nd+1
        select case (this%mechanic_bnd)
          case('frees')
            matica(4,is) = +sqrt((j-1)/(2*(2*j+1)))
            matica(5,is) = -sqrt((j+2)/(2*(2*j+1)))
            
          case('noslp')
            matica(3,is) = grid%c(ir,-1)
            matica(6,is) = grid%c(ir,+1)
        end select
    
    end associate
  
  end procedure mat_torr_fn
  
end submodule toroidal_visc_matrix