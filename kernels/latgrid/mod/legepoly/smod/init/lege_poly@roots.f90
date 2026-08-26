submodule (lege_poly) roots
  implicit none; contains
  
  module procedure find_roots_sub
    integer        :: i
    real(kind=qbl) :: x1, fx1, x2, fx2, x3, fx3, root, froot
    
    !!**********************************************************************!!
    !!* Close to roots array holder and holder arrays.                     *!!
    !!**********************************************************************!!
    call alloc_aligned_sub( this%nLege, this%c_cosx,  this%cosx  )
    call alloc_aligned_sub( this%nLege, this%c_sinx,  this%sinx  )
    call alloc_aligned_sub( this%nLege, this%c_cosx2, this%cosx2 )
    call alloc_aligned_sub( this%nLege, this%c_wght,  this%wght  )
    
    !!**********************************************************************!!
    !!* Riddler method with bracketing from Stjeltjes.                     *!!
    !!**********************************************************************!!
    !$omp parallel do private (x1,fx1,x2,fx2,x3,fx3,root,froot)
    do i = 1, this%nLege
      x1  = cos( (i-0.5_qbl) * qpi / (2*this%nLege) )
      fx1 = lege_fn(2*this%nLege, x1)
      
      x2  = cos( i * qpi / (2*this%nLege+1) )
      fx2 = lege_fn(2*this%nLege, x2)
      
      do
        x3  = ( x1 + x2 ) / 2
        fx3 = lege_fn(2*this%nLege, x3)
        
        root  = x3 + (x3-x1) * sign(1._qbl,fx1-fx2) * fx3 / sqrt( fx3**2 - fx1*fx2 )
        froot = lege_fn(2*this%nLege, root)
        
        if ( abs(froot) < qeps ) then
          exit
        else if ( fx3 * froot < qzero ) then
          x1  = x3
          fx1 = fx3
          x2  = root
          fx2 = froot
        else if ( fx1 * froot < qzero ) then
          x1  = root
          fx1 = froot
        else if ( fx2 * froot < qzero ) then
          x2  = root
          fx2 = froot
        end if
      end do
      
      this%cosx(i)  = q2r_fn( root )
      this%sinx(i)  = q2r_fn( sqrt( 1 - root**2 ) )
      this%cosx2(i) = q2r_fn( root**2 )
      this%wght(i)  = q2r_fn( qpi * (1-root**2) / ( this%nLege * lege_fn(2*this%nLege-1, root) )**2 )
    end do
    !$omp end parallel do
    
  end procedure find_roots_sub
  
end submodule roots