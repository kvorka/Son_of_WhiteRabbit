submodule (sphsvt2) shtns_to_jm
  implicit none; contains
  
  module procedure shtns_to_jm_sphsvt2_sub
    integer                        :: ij, im, ijm, iv
    complex(kind=dbl), allocatable :: temporal(:)
    
    allocate( temporal(this%jms) )
    
    do ij = 0, this%jmax
      im = 0
        temporal(jm(ij,im)) = r2c_fn( arr(shtns_lmidx(shtns_c,ij,im))%re )
      
      do im = 0, ij
        temporal(jm(ij,im)) = arr(shtns_lmidx(shtns_c,ij,im))
      end do
    end do
    
    !$omp simd
    do ijm = 1, this%jms
      arr(ijm) = temporal(ijm)
    end do
    
    deallocate( temporal )
    
  end procedure shtns_to_jm_sphsvt2_sub
  
end submodule shtns_to_jm