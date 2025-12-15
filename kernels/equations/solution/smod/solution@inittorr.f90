submodule (solution) inittorr
  implicit none; contains
  
  module procedure init_storr_sub
    integer :: ij
    
    allocate( this%torr(1:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 1, this%jmax
      call this%torr(ij)%init_sub(ij,2*this%nd+1)
    end do
    !$omp end parallel do
    
  end procedure init_storr_sub
  
end submodule inittorr