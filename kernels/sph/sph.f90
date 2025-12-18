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
    
    module function scalnorm2_fn(np, cajm) result(sp)
      integer,           intent(in) :: np
      complex(kind=dbl), intent(in) :: cajm(*)
      real(kind=dbl)                :: sp
    end function scalnorm2_fn
    
    module function vectnorm2_fn(np, cajml) result(vp)
      integer,           intent(in) :: np
      complex(kind=dbl), intent(in) :: cajml(*)
      real(kind=dbl)                :: vp
    end function vectnorm2_fn
    
    module subroutine ezvv_sub(np, fac, cajml, cjml)
      integer,           intent(in)  :: np
      real(kind=dbl),    intent(in)  :: fac
      complex(kind=dbl), intent(in)  :: cajml(*)
      complex(kind=dbl), intent(out) :: cjml(3,*)
    end subroutine ezvv_sub
  end interface
  
end module sph