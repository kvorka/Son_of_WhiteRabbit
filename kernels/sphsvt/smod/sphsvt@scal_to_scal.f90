submodule (sphsvt) scal_to_scal
  implicit none; contains
  
  module procedure scal2scal_mj_to_jm_sub
    integer :: j, m, ijm, imj
    
    do j = 0, this%jmax
      m = 0
        ijm = j*(j+1)/2+m+1
        imj = m*this%jmax2-m*(m+1)/2+j+1
          cjm(ijm)%re = cr(crpadding,imj)%re
          cjm(ijm)%im = zero
      
      do m = 1, j
        ijm = j*(j+1)/2+m+1
        imj = m*this%jmax2-m*(m+1)/2+j+1
          cjm(ijm) = cr(crpadding,imj)
      end do
    end do
    
  end procedure scal2scal_mj_to_jm_sub
  
end submodule scal_to_scal