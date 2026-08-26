submodule (output_mod) nuss_curve
  implicit none; contains
  
  module procedure nuss_curve_sub
    integer         :: n, error
    real(kind=dbl)  :: poc, t, dt, Nuss, Re, sumNuss, sumRe, sumEk
    
    n = 0
    
    sumNuss = zero
    sumRe   = zero
    sumEk   = zero
    
    open(unit=1, file=path_nuss, status='old', action='read')
      
      do
        read(1,*,iostat=error) poc, t, Nuss, Re
        
        if ( error /= 0 ) then
          exit
        else if ( t > tNuss ) then
          n = n + 1
          
          sumNuss  = sumNuss + Nuss
          sumRe    = sumRe + Re
          sumEk    = sumEk + Re**2
        end if
      end do
      
    close(1)
    
    open( unit=8, file='nuss', status='new', action='write' )
      write(8,*) sumNuss / n , sumRe / n , sqrt( sumEk / n )
    close(8)
    
  end procedure nuss_curve_sub
  
end submodule nuss_curve