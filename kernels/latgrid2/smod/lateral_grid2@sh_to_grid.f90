submodule (lateral_grid2) sh_to_grid
  implicit none; contains
  
  module procedure q2grid_shtns_sub
    integer                        :: ij, im, ijm
    complex(kind=dbl), allocatable :: temporal(:)
    
    allocate( temporal(this%jms) )
      
      do ij = 0, this%jmax
        do im = 0, ij
          temporal(shtns_lmidx(this%shtns_c,ij,im)) = scal(jm(ij,im))
        end do
      end do
      
      !$omp simd
      do ijm = 1, this%jms
        scal(ijm) = temporal(ijm)
      end do
      
    deallocate( temporal )
    
    call SH_to_spat( this%shtns_c, scal, grid )
    
  end procedure q2grid_shtns_sub
  
  module procedure qst2grid_shtns_sub
    integer                        :: ij, im, ijm
    complex(kind=dbl), allocatable :: temporal(:)
    
    allocate( temporal(this%jms) )
      
      do iv = 1, 3
        do ij = 0, this%jmax
          do im = 0, ij
            temporal(shtns_lmidx(this%shtns_c,ij,im)) = vec(jm(ij,im),iv)
          end do
        end do
        
        !$omp simd
        do ijm = 1, this%jms
          vec(ijm,iv) = temporal(ijm)
        end do
      end do
      
    deallocate( temporal )
    
    call SHqst_to_spat( this%shtns_c, vec(1,1), vec(1,3), vec(1,2), grid(1,1), tgrid(1,2), pgrid(1,3) )
    
  end procedure qst2grid_shtns_sub
  
end submodule sh_to_grid