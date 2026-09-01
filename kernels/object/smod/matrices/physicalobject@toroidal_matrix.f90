submodule (physicalobject) toroidal_visc_matrix
  implicit none ; contains
  
  module procedure prepare_mat_torr_sub
    integer :: ij
    
    !$omp parallel do
    do ij = 0, this%jmax
      call this%mat_torr_sub( ij, this%cf  , this%torr(ij)%U )
      call this%mat_torr_sub( ij, this%cf-1, this%torr(ij)%M )
      
      call this%torr(ij)%luDecompose_sub()
    end do
    !$omp end parallel do
    
  end procedure prepare_mat_torr_sub
  
  module procedure mat_torr_sub
    integer        :: ir, is
    real(kind=dbl) :: fac
    
    matica = zero
    
    ir = 1
      is = 1
        select case ( this%mechanical_bnd )
          case ('noslp')
            matica(4,is) = this%rad_grid%c(ir,-1)
            matica(6,is) = this%rad_grid%c(ir,+1)
          
          case ('frslp')
            matica(4,is) = -this%rad_grid%r(ir) * this%rad_grid%c(ir,-1)
            matica(5,is) = +one
            matica(6,is) = -this%rad_grid%r(ir) * this%rad_grid%c(ir,+1)
        end select
    
    do ir = 1, this%nd
      is  = 2*(ir-1)+1
      
      if ( ir > 1 ) then
        fac = 1 / this%rad_grid%rr(ir)**2
        
        matica(1,is) = a * this%rad_grid%dd(ir,-2) * fac
        matica(3,is) = a * this%rad_grid%dd(ir,-1) * fac
        matica(4,is) = -1 / ( this%Pr * this%dt ) - a * this%hdiff_fn(j) * j*(j+1) * fac
        matica(5,is) = a * this%rad_grid%dd(ir,+1) * fac
        matica(7,is) = a * this%rad_grid%dd(ir,+2) * fac
      end if
      
      matica(1,is+1) = -this%rad_grid%d(ir,-2)
      matica(3,is+1) = -this%rad_grid%d(ir,-1)
      matica(4,is+1) = 1 / this%rad_grid%r(ir)**2
      matica(5,is+1) = -this%rad_grid%d(ir,+1)
      matica(7,is+1) = -this%rad_grid%d(ir,+2)
    end do
    
    ir = this%nd
      is = 2*ir+1
        select case ( this%mechanical_bnd )
          case ('noslp')
            matica(2,is) = this%rad_grid%c(ir,-1)
            matica(4,is) = this%rad_grid%c(ir,+1)
          
          case ('frslp')
            matica(2,is) = -this%rad_grid%r(ir) * this%rad_grid%c(ir,-1)
            matica(3,is) = +one
            matica(4,is) = -this%rad_grid%r(ir) * this%rad_grid%c(ir,+1)
        end select
        
  end procedure mat_torr_sub
  
end submodule toroidal_visc_matrix