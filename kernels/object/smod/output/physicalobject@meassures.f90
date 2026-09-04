submodule (physicalobject) meassures
  implicit none ; contains
  
  module procedure nuss_fn
    
    select case (this%thermal_bnd)
      case ('basic')
        nuss_fn = -real( this%dT_dr_r_fn(this%nd,0,0), kind=dbl ) / ( this%r_ud * s4pi )
      
      case ('fluxd')
        nuss_fn = real( this%temp_r_fn(1,0,0), kind=dbl ) / s4pi
      
    end select
    
  end procedure nuss_fn
  
  module procedure reynolds_fn
    integer                        :: ir, ij, ij01
    real(kind=dbl),    allocatable :: field_vals(:)
    complex(kind=dbl), allocatable :: velocity(:)
    
    allocate( field_vals(this%nd+1) )
    
    if ( ( present(choice) ) .and. ( choice == 'convective' ) ) then
      
      !$omp parallel private (velocity)
      allocate( velocity(this%jmv) )
      
      !$omp do private (ij)
      do ir = 1, this%nd+1
        call this%velc_rr_jml_sub( ir, velocity )
        
        do ij = 1, this%jmax
          ij01 = jml(ij,0,-1)
          
          velocity( ij01   ) = czero
          velocity( ij01+1 ) = czero
          velocity( ij01+2 ) = czero
        end do
        
        field_vals(ir) = vectnorm2_fn( this%jmax, velocity )
      end do
      
      deallocate( velocity )
      !$omp end parallel
      
    else
      
      !$omp parallel private (velocity)
      allocate( velocity(this%jmv) )
      
      !$omp do
      do ir = 1, this%nd+1
        call this%velc_rr_jml_sub( ir, velocity )
        field_vals(ir) = vectnorm2_fn( this%jmax, velocity )
      end do
      
      deallocate( velocity )
      !$omp end parallel
      
    end if
    
    reynolds_fn = sqrt( this%rad_grid%intV_fn( field_vals ) / this%rad_grid%volume )
    
    deallocate( field_vals )
    
  end procedure reynolds_fn
  
end submodule meassures