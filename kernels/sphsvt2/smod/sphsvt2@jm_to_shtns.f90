submodule (sphsvt2) jm_to_shtns
  implicit none; contains
  
  module procedure jm_to_shtns_sphsvt2_sub
    integer                        :: ij, im, ijm
    complex(kind=dbl), allocatable :: temporal(:)
    
    allocate( temporal(this%jms) )
    
    do ij = 0, this%jmax
      im = 0
        temporal(shtns_lmidx(shtns_c,ij,im)) = r2c_fn( arr(jm(ij,im) )%re )
      
      do im = 1, ij
        temporal(shtns_lmidx(shtns_c,ij,im)) = arr(jm(ij,im))
      end do
    end do
    
    !$omp simd
    do ijm = 1, this%jms
      arr(ijm) = temporal(ijm)
    end do
    
    deallocate( temporal )
    
  end procedure jm_to_shtns_sphsvt2_sub
  
end submodule jm_to_shtns