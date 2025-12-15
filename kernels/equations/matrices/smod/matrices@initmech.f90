submodule (Matrices) initmech
  implicit none; contains
  
  module procedure init_mmech_sub
    integer :: j
    
    allocate( this%mech(1:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do j = 1, this%jmax
      call this%mech(j)%init_sub(5*this%nd+2, 9, 8)
    end do
    !$omp end parallel do
    
  end procedure init_mmech_sub
  
end submodule initmech