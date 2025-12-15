submodule (rhs) inittemp
  implicit none; contains
  
  module procedure init_rtemp_sub
    integer :: ij
    
    allocate( this%temp(0:this%jmax) )
    
    !$omp parallel do schedule(guided,2)
    do ij = 0, this%jmax
      call this%temp(ij)%init_sub(ij,this%nd+1)
    end do
    !$omp end parallel do
    
  end procedure init_rtemp_sub
  
end submodule inittemp