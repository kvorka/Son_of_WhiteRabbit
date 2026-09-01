submodule (physicalobject) globrot
  implicit none; contains
  
  module procedure substract_globrot_sub
    integer                        :: ir, is
    real(kind=dbl)                 :: coeff
    complex(kind=dbl)              :: angularMomentum0, angularMomentum1
    complex(kind=dbl), allocatable :: angularMomentum_rr(:,:), v_rr(:,:)
    
    coeff = 5 * ( ( 1 / this%r_ud-1 )**5 ) / ( 1 / this%r_ud**5-1 )
    
    allocate( v_rr(this%nd+1,0:1) )
      
      call this%velc_jml_rr_sub( 1, 0, 0, v_rr(1,0) )
      call this%velc_jml_rr_sub( 1, 1, 0, v_rr(1,1) )
      
    allocate( angularMomentum_rr(this%nd+1,0:1) )
      
      !$omp simd
      do ir = 1, this%nd+1
        angularMomentum_rr(ir,0) = this%rad_grid%rr(ir) * v_rr(ir,0)
        angularMomentum_rr(ir,1) = this%rad_grid%rr(ir) * v_rr(ir,1)
      end do
    
    deallocate( v_rr )
      
      angularMomentum0 = coeff * this%rad_grid%intV_fn( angularMomentum_rr(:,0) )
      angularMomentum1 = coeff * this%rad_grid%intV_fn( angularMomentum_rr(:,1) )
      
    deallocate( angularMomentum_rr )
    
    !$omp simd
    do ir = 1, this%nd+1
      is = 2*(ir-1)+1
      
      this%torr(1)%sol(0,is) = this%torr(1)%sol(0,is) - angularMomentum0 * this%rad_grid%rr(ir)
      this%torr(1)%sol(1,is) = this%torr(1)%sol(1,is) - angularMomentum1 * this%rad_grid%rr(ir)
    end do
    
  end procedure substract_globrot_sub
  
end submodule globrot