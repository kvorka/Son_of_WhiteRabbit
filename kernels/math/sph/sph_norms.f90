module sph_norms
  use math
  implicit none; public; contains
  
  pure function scalnorm2_fn(np, cajm) result(sp)
    integer,           intent(in) :: np
    complex(kind=dbl), intent(in) :: cajm(*)
    integer                       :: j, m, indx
    real(kind=dbl)                :: sp
    
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
    
  end function scalnorm2_fn
  
  pure function vectnorm2_fn(np, cajml) result(vp)
    integer,           intent(in) :: np
    complex(kind=dbl), intent(in) :: cajml(*)
    integer                       :: j, m, indx
    real(kind=dbl)                :: vp
    
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
    
  end function vectnorm2_fn
  
end module sph_norms