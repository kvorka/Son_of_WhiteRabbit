submodule (sphsvt) scal_to_scal
  implicit none; contains
  
  module procedure scal2scal_mj_to_jm_sub
    integer :: ij, im
    
    do ij = 0, this%jmax
      im = 0
        cjm(jm(ij,0))%re = r2c_fn( cr(crpadding,im*this%jmax2-im*(im+1)/2+ij+1)%re )
      
      !$omp simd
      do im = 1, ij
        cjm(jm(ij,im)) = cr(crpadding,im*this%jmax2-im*(im+1)/2+ij+1)
      end do
    end do
    
  end procedure scal2scal_mj_to_jm_sub
  
end submodule scal_to_scal