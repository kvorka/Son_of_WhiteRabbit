submodule (lege_poly) lege_func
  implicit none; contains
  
  module procedure lege_fn
    integer        :: i
    real(kind=qbl) :: p1, p2
    
    p1      = qone
    lege_fn = x
    
    do i = 2, deg
      p2      = ( 2 - qone / i ) * ( lege_fn * x - p1 ) + p1
      p1      = lege_fn
      lege_fn = p2
    end do
    
  end procedure lege_fn
  
end submodule lege_func