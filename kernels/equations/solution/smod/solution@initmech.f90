submodule (solution) initmech
  implicit none; contains
  
  module procedure init_smech_sub
    integer :: ij
    
    allocate( this%mech(1:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 1, this%jmax
      call this%mech(ij)%init_sub(ij,5*this%nd+2)
    end do
    !$omp end parallel do
    
  end procedure init_smech_sub
  
end submodule initmech