submodule (sph) norms
  implicit none; contains
  
  module procedure scalnorm2_fn
    integer :: j, m, indx
    
    !j == 0
    sp = c2r_fn( cajm(1) * conjg( cajm(1) ) )
    
    !higher degrees
    do j = 1, np
      !m == 0
      indx = j*(j+1)/2+1
      sp   = sp + c2r_fn( cajm(indx) * conjg(cajm(indx)) )
      
      do m = 1, j
        indx = j*(j+1)/2+m+1
        sp   = sp + 2 * c2r_fn( cajm(indx) * conjg(cajm(indx)) )
      end do
    end do
    
  end procedure scalnorm2_fn
  
  module procedure vectnorm2_fn
    integer :: j, m, indx
    
    !j == 0
    vp = c2r_fn( cajml(1) * conjg(cajml(1)) )
    
    !higher degrees
    do j = 1, np
      !m == 0
      indx = 3*(j*(j+1)/2)-1
      vp   = vp + sum( c2r_fn( cajml(indx:indx+2) * conjg(cajml(indx:indx+2)) ) )
      
      do m = 1, j
        indx = 3*(j*(j+1)/2+m)-1
        vp   = vp + 2 * sum( c2r_fn( cajml(indx:indx+2) * conjg(cajml(indx:indx+2)) ) )
      end do
    end do
    
  end procedure vectnorm2_fn
  
end submodule norms