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
  
  module procedure bnd_flux_fn
    
    bnd_flux_fn = real( this%dT_dr_r_fn(this%nd,0,0), kind=dbl ) / real( this%dT_dr_r_fn(1,0,0), kind=dbl ) / this%r_ud**2
    
  end procedure bnd_flux_fn
  
  module procedure reynolds_fn
    integer                        :: ir
    real(kind=dbl),    allocatable :: field_vals(:)
    complex(kind=dbl), allocatable :: velocity(:)
    
    allocate( field_vals(this%nd+1), velocity(this%jmv) )
    
    !$omp parallel do private (velocity)
    do ir = 1, this%nd+1
      call this%velc_rr_jml_sub( ir, velocity )
      field_vals(ir) = vectnorm2_fn( this%jmax, velocity )
    end do
    !$omp end parallel do
    
    reynolds_fn = sqrt( this%rad_grid%intV_fn( field_vals ) / this%rad_grid%volume )
    
    deallocate( field_vals, velocity )
    
  end procedure reynolds_fn
  
  module procedure reynolds_c_fn
    integer                        :: ir
    real(kind=dbl),    allocatable :: field_vals(:)
    complex(kind=dbl), allocatable :: velocity(:)
    
    allocate( field_vals(this%nd+1), velocity(this%jmv) )
    
    !$omp parallel do private (velocity)
    do ir = 1, this%nd+1
      call this%velc_rr_jml_sub( ir, velocity )
      field_vals(ir) = vectnorm2_c_fn( this%jmax, velocity )
    end do
    !$omp end parallel do
    
    reynolds_c_fn = sqrt( this%rad_grid%intV_fn( field_vals ) / this%rad_grid%volume )
    
    deallocate( field_vals, velocity )
    
  end procedure reynolds_c_fn
  
  module procedure reynolds_z_fn
    integer                        :: ir
    real(kind=dbl),    allocatable :: field_vals(:)
    complex(kind=dbl), allocatable :: velocity(:)
    
    allocate( field_vals(this%nd+1), velocity(this%jmv) )
    
    !$omp parallel do private (velocity)
    do ir = 1, this%nd+1
      call this%velc_rr_jml_sub( ir, velocity )
      field_vals(ir) = vectnorm2_z_fn( this%jmax, velocity )
    end do
    !$omp end parallel do
    
    reynolds_z_fn = sqrt( this%rad_grid%intV_fn( field_vals ) / this%rad_grid%volume )
    
    deallocate( field_vals, velocity )
    
  end procedure reynolds_z_fn
  
end submodule meassures