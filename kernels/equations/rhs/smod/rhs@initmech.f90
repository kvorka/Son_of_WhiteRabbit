submodule (rhs) initmech
  implicit none; contains
  
  module procedure init_rmech_sub
    integer :: ij
    
    allocate( this%sph1(1:this%jmax), this%sph2(1:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 1, this%jmax
      call this%sph1(ij)%init_sub(ij,this%nd+1)
      call this%sph2(ij)%init_sub(ij,this%nd+1)
    end do
    !$omp end parallel do
    
  end procedure init_rmech_sub
  
end submodule initmech