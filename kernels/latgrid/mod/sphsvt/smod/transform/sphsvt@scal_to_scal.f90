submodule (sphsvt) scal_to_scal
  implicit none; contains
  
  module procedure scal2scal_mj_to_jm_sub
    integer :: ij, im, ijm, imj
    
    do ij = 0, this%jmax
      im = 0
        ijm = jm(ij,im)
        imj = mj(this%jmax1,im,ij)
        
        cjm(ijm)%re = cr(imj)%re
        cjm(ijm)%im = zero

      
      !$omp simd
      do im = 1, ij
        ijm = jm(ij,im)
        imj = mj(this%jmax1,im,ij)
        
        cjm(ijm) = cr(imj)
      end do
    end do
    
  end procedure scal2scal_mj_to_jm_sub
  
end submodule scal_to_scal