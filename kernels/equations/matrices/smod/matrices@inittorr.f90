submodule (matrices) inittorr
  implicit none; contains
  
  module procedure init_mtorr_sub
    integer :: j
    
    allocate( this%torr(1:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do j = 1, this%jmax
      call this%torr(j)%init_sub(2*this%nd+1, 3, 3)
    end do
    !$omp end parallel do
    
  end procedure init_mtorr_sub
  
end submodule inittorr