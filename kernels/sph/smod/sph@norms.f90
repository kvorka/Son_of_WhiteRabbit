submodule (sph) norms
  implicit none; contains
  
  module procedure vectnorm2_fn
    integer :: ij, iml, ij00
    
    !ij = 0
      !im = 0; il = 1
        vp = cajml(1)%re**2
    
    do ij = 1, np
      ij00 = jml(ij,0,0)
      
      !im = 0; il = -1,0,1
        vp = vp + cajml(ij00-1)%re**2 + &
                & cajml(ij00  )%im**2 + &
                & cajml(ij00+1)%re**2
      
      !$omp simd reduction (+:vp)
      do iml = 2, 3*ij+1
        vp = vp + 2 * ( cajml(ij00+iml)%re**2 + cajml(ij00+iml)%im**2 )
      end do
    end do
    
  end procedure vectnorm2_fn
  
end submodule norms