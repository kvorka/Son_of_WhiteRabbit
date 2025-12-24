submodule (sphsvt2) jml_to_qst
  implicit none; contains
  
  module procedure jml_to_qst_sphsvt2_sub
    integer                        :: ij, im, ij0
    real(kind=dbl)                 :: cs1, cs2, cq1, cq2
    complex(kind=dbl)              :: ct1
    complex(kind=dbl), allocatable :: vtmp(:)
    
    allocate( vtmp(0:this%jmax) )
    
    !ij = 0
      !ij0 = 1
      
      pol1(1) = -pol2(1)
      pol2(1) = +pol2(1)
      torr(1) = czero
      
    do ij = 1, this%jmax
      ij0 = jm(ij,0)
      
      ct1 = cunit / sqrt( ij * ( ij+one ) )
      
      cs1 = 1 / sqrt( ( ij   ) * ( 2*ij+one ) )
      cs2 = 1 / sqrt( ( ij+1 ) * ( 2*ij+one ) )
      
      cq1 = +( ij   ) * cs1
      cq2 = -( ij+1 ) * cs2
      
      call copy_carray_sub( ij+1, pol1(ij0), vtmp(0) )
      
      !$omp simd
      do im = 0, ij
        pol1(ij0+im) = cq1 * vtmp(    im) + cq2 * pol2(ij0+im)
        pol2(ij0+im) = cs1 * vtmp(    im) + cs2 * pol2(ij0+im)
        torr(ij0+im) = ct1 * torr(ij0+im)
      end do
    end do
    
    deallocate( vtmp )
    
  end procedure jml_to_qst_sphsvt2_sub
  
end submodule jml_to_qst