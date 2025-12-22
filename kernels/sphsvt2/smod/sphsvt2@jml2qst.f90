submodule (sphsvt2) jml2qst
  implicit none; contains
  
  module procedure jml2qst_sphsvt2_sub
    integer           :: ij, ijm
    real(kind=dbl)    :: cs1, cs2, cq1, cq2
    complex(kind=dbl) :: ct1, vtemp
    
    do ij = 1, this%jmax
      ct1 = cunit / sqrt( ij * ( ij+one ) )
      
      cs1 = 1 / sqrt( ( ij   ) * ( 2*ij+one ) )
      cs2 = 1 / sqrt( ( ij+1 ) * ( 2*ij+one ) )
      
      cq1 = +( ij   ) * cs1
      cq2 = -( ij+1 ) * cs2
      
      !$omp simd
      do ijm = jm(ij,0), jm(ij,ij)
        vtemp = vec(ijm,1)
        
        vec(ijm,1) = cq1 * vec(ijm,1) + cq2 * vec(ijm,3)
        vec(ijm,3) = cs1 * vtemp      + cs2 * vec(ijm,3)
        vec(ijm,2) = ct1 * vec(ijm,2)
      end do
    end do
    
  end procedure jml2qst_sphsvt2_sub
  
end submodule jml2qst