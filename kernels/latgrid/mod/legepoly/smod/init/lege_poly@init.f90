submodule (lege_poly) init
  implicit none; contains
  
  module real(kind=qbl) function lege_fn(deg, x)
    integer,        intent(in) :: deg
    real(kind=qbl), intent(in) :: x
    integer                    :: i
    real(kind=qbl)             :: p1, p2
    
    p1      = qone
    lege_fn = x
    
    do i = 2, deg
      p2      = ( 2 - qone / i ) * ( lege_fn * x - p1 ) + p1
      p1      = lege_fn
      lege_fn = p2
    end do
    
  end function lege_fn
  
  module procedure init_lege_sub
    integer        :: i, ij, im, imj, ima
    real(kind=qbl) :: x1, fx1, x2, fx2, x3, fx3, root, froot
    
    !!**********************************************************************!!
    !!* Initialize the size of the transform.                              *!!
    !!**********************************************************************!!
    this%nLege = nLege
    this%jmax  = jmax
    
    !!**********************************************************************!!
    !!* Initialize the weird degree counter and bounds.                    *!!
    !!**********************************************************************!!
    this%nrma = 0
      do im = 0, this%jmax
        this%nrma = this%nrma+1
        
        if ( im < this%jmax ) then
          do ij = 1, (this%jmax-1-im)/2
            this%nrma = this%nrma+1
          end do
          
          this%nrma = this%nrma+1
        end if
      end do
    
    allocate( this%mamj(0:this%jmax) )
    
    ima = 0
    
    do im = 0, this%jmax
      !j = m
        ima = ima+1
        this%mamj(im) = ima
      
      do ij = 1, (this%jmax-im)/2
        ima = ima+1
      end do
      
      if ( mod((this%jmax-im),2) /= 0 ) then
        ima = ima+1
      end if
    end do
    
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
        
        root  = x3 + (x3-x1) * sign(qone,fx1-fx2) * fx3 / sqrt( fx3**2 - fx1*fx2 )
        froot = lege_fn(2*this%nLege, root)
        
        if ( abs(froot) < 1.0d-28 ) then
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
      
      this%cosx(i)  = real( root, kind=dbl )
      this%sinx(i)  = real( sqrt( 1 - root**2 ), kind=dbl )
      this%cosx2(i) = real( root**2, kind=dbl )
      this%wght(i)  = real( qpi * (1-root**2) / ( this%nLege * lege_fn(2*this%nLege-1, root) )**2, kind=dbl ) / wfac
    end do
    !$omp end parallel do
    
    !!**********************************************************************!!
    !!* Initialize the recursion coefficients.                             *!!
    !!**********************************************************************!!
    allocate( this%emj((this%jmax+3)*(this%jmax+2)/2) )
    
    do im = 0, this%jmax+1
      do ij = im, this%jmax+1
        this%emj(im*(this%jmax+2)-im*(im+1)/2+ij+1) = real( sqrt((ij**2-im**2)/(4*ij**2-qone)), kind=dbl )
      end do
    end do
    
    allocate( this%fmj(3,this%nrma) ) ; ima = 0
    
    do im = 0, this%jmax
      !j = m
        imj = im*(this%jmax+2)-(im-2)*(im+1)/2
        ima = ima+1
        
        if ( im == 0 ) then
          this%fmj(1,ima) = real( qone / sqrt(4*pi), kind=dbl )
        else
          this%fmj(1,ima) = real( -sqrt( (2*im+qone) / (2*im) ), kind=dbl )
        end if
      
      if ( im < this%jmax ) then
        do ij = 1, (this%jmax-im)/2
          imj = imj+2
          ima = ima+1
          
          if ( ij == 1 ) then
            this%fmj(1,ima) =               1 / ( this%emj(imj) * this%emj(imj-1) )
            this%fmj(2,ima) = this%emj(imj-1) / ( this%emj(imj)                   )
            this%fmj(3,ima) = zero
          else
            this%fmj(1,ima) =                                                             1 / ( this%emj(imj) * this%emj(imj-1) )
            this%fmj(2,ima) = ( this%emj(imj-1)**2 + this%emj(imj-2)**2                   ) / ( this%emj(imj) * this%emj(imj-1) )
            this%fmj(3,ima) = (                      this%emj(imj-2)    * this%emj(imj-3) ) / ( this%emj(imj) * this%emj(imj-1) )
          end if
        end do
        
        if ( mod((this%jmax-im),2) /= 0 ) then
          imj = imj+2
          ima = ima+1
          
          if ( im == this%jmax-1 ) then
            this%fmj(1,ima) =               1 / ( this%emj(imj) * this%emj(imj-1) )
            this%fmj(2,ima) = this%emj(imj-1) / ( this%emj(imj)                   )
            this%fmj(3,ima) = zero
          else
            this%fmj(1,ima) =                                                             1 / ( this%emj(imj) * this%emj(imj-1) )
            this%fmj(2,ima) = ( this%emj(imj-1)**2 + this%emj(imj-2)**2                   ) / ( this%emj(imj) * this%emj(imj-1) )
            this%fmj(3,ima) = (                      this%emj(imj-2)    * this%emj(imj-3) ) / ( this%emj(imj) * this%emj(imj-1) )
          end if
        end if
      end if
    end do
    
  end procedure init_lege_sub
  
end submodule init