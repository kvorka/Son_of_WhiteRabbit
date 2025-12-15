submodule (rhs) inittorr
  implicit none; contains
  
  module procedure init_rtorr_sub
    integer :: ij
    
    allocate( this%torr(0:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 0, this%jmax
      call this%torr(ij)%init_sub(ij,this%nd+1)
    end do
    !$omp end parallel do
    
  end procedure init_rtorr_sub
  
end submodule inittorr