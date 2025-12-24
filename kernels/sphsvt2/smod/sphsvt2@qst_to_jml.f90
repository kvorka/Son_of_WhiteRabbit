submodule (sphsvt2) qst_to_jml
  implicit none; contains
  
  module procedure qst_to_jml_sphsvt2_sub
    integer                        :: ij, im, ij0
    real(kind=dbl)                 :: cs1, cs2, cq1, cq2
    complex(kind=dbl)              :: ct1
    complex(kind=dbl), allocatable :: vtmp(:)
    
    allocate( vtmp(0:this%jmax) )
    
    do ij = 0, this%jmax
      ij0 = jm(ij,0)
      
      ct1 = -cunit * sqrt( ij * ( ij+one ) )
      
      cq1 = +sqrt( (ij  ) / (2*ij+one) )
      cq2 = -sqrt( (ij+1) / (2*ij+one) )
      
      cs1 = +(ij+1) * cq1
      cs2 = -(ij  ) * cq2
      
      call copy_carray_sub( ij+1, pol1(ij0), vtmp(0) )
      
      !$omp simd
      do im = 0, ij
        pol1(ij0+im) = cq1 * vtmp(    im) + cs1 * pol2(ij0+im)
        pol2(ij0+im) = cq2 * vtmp(    im) + cs2 * pol2(ij0+im)
        torr(ij0+im) = ct1 * torr(ij0+im)
      end do
    end do
    
    deallocate( vtmp )
    
  end procedure qst_to_jml_sphsvt2_sub
  
end submodule qst_to_jml