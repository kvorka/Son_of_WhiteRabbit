module sph
  use math
  implicit none; public
  
  interface
    module integer function jm(ij, im)
      integer, intent(in) :: ij, im
    end function jm
    
    module integer function jml(ij, im, il)
      integer, intent(in) :: ij, im, il
    end function jml
    
    module integer function mj(ijmax, im, ij)
      integer, intent(in) :: ijmax, im, ij
    end function mj
    
    module function vectnorm2_fn(np, cajml) result(vp)
      integer,           intent(in) :: np
      complex(kind=dbl), intent(in) :: cajml(*)
      real(kind=dbl)                :: vp
    end function vectnorm2_fn
  end interface
  
end module sph