submodule (radial_grid) interpolation
  implicit none ; contains
  
  module procedure interpolation_sub
    integer        :: ijm, ir1, jmmax
    real(kind=dbl) :: cr1, cr2
    
    jmmax = min( jmdim1, jmdim )          !! figure out, which sequence is longer
    call zero_carray_sub( jmdim, field )  !! zero the holder for extrapolation results
    
    if ( ir == 1 ) then
      call copy_carray_sub( jmmax, field1(1,1), field(1) )
    
    else if ( ir == this%nd+1 ) then
      call copy_carray_sub( jmmax, field1(1,nrdim1), field(1) )
    
    else
      do ir1 = 1, nrdim1-1
        if ( ( this%rr(ir) >= rr1(ir1) ) .and. ( this%rr(ir) <= rr1(ir1+1) ) ) then
          cr1 = ( this%rr(ir) - rr1(ir1)    ) / ( rr1(ir1+1) - rr1(ir1) )
          cr2 = ( rr1(ir1+1)  - this%rr(ir) ) / ( rr1(ir1+1) - rr1(ir1) )
          
          !$omp simd
          do ijm = 1, jmmax
            field(ijm) = cr1 * field1(ijm,ir1+1) + cr2 * field1(ijm,ir1)
          end do
          
          exit
        end if
      end do
    end if
    
  end procedure interpolation_sub
  
end submodule interpolation
