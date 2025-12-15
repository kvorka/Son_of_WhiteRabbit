submodule (rhs) initmech
  implicit none; contains
  
  module procedure init_rmech_sub
    integer :: ij
    
    allocate( this%sph1(0:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 0, this%jmax
      call this%sph1(ij)%init_sub(ij,this%nd+1)
    end do
    !$omp end parallel do

    allocate( this%sph2(0:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 0, this%jmax
      call this%sph2(ij)%init_sub(ij,this%nd+1)
    end do
    !$omp end parallel do
    
  end procedure init_rmech_sub
  
end submodule initmech