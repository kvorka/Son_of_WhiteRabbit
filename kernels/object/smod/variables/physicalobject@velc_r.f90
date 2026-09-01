submodule (physicalobject) velc_r
  implicit none; contains
  
  module procedure velc_jml_rr_sub
    integer :: ir
    
    select case (il)
      case (-1)
        !$omp simd
        do ir = 1, this%nd+1
          v_rr(ir) = this%mech(ij)%sol(im,5*(ir-1)+1)
        end do
      
      case (0)
        !$omp simd
        do ir = 1, this%nd+1
          v_rr(ir) = this%torr(ij)%sol(im,2*(ir-1)+1)
        end do
      
      case (+1)
        !$omp simd
        do ir = 1, this%nd+1
          v_rr(ir) = this%mech(ij)%sol(im,5*(ir-1)+2)
        end do
    end select
    
  end procedure velc_jml_rr_sub
  
end submodule velc_r