submodule (physicalobject) equations_all
  implicit none ; contains
  
  module procedure init_eq_all_sub
    integer :: ij
    
    allocate( this%mat%temp(0:this%jmax), this%sol%temp(0:this%jmax), this%rhs%temp(0:this%jmax) )
    allocate( this%mat%torr(1:this%jmax), this%sol%torr(1:this%jmax), this%rhs%torr(1:this%jmax) )
    allocate( this%mat%mech(1:this%jmax), this%sol%mech(1:this%jmax), this%rhs%sph1(1:this%jmax), this%rhs%sph2(1:this%jmax) )
    
    !$omp parallel
    
    !$omp do schedule (guided,2)
    do ij = 0, this%jmax
      call this%mat%temp(ij)%init_sub(2*this%nd+1, 3, 3)
      call this%sol%temp(ij)%init_sub(ij,2*this%nd+1)
      call this%rhs%temp(ij)%init_sub(ij,  this%nd+1)
    end do
    !$omp end do nowait
    
    !$omp do schedule (guided,2)
    do ij = 1, this%jmax
      call this%mat%torr(ij)%init_sub(2*this%nd+1, 3, 3)
      call this%sol%torr(ij)%init_sub(ij,2*this%nd+1)
      call this%rhs%torr(ij)%init_sub(ij,  this%nd+1)
    end do
    !$omp end do nowait
    
    !$omp do schedule (guided,2)
    do ij = 1, this%jmax
      call this%mat%mech(ij)%init_sub(5*this%nd+2, 9, 8)
      call this%sol%mech(ij)%init_sub(ij,5*this%nd+2)
      call this%rhs%sph1(ij)%init_sub(ij,  this%nd+1)
      call this%rhs%sph2(ij)%init_sub(ij,  this%nd+1)
    end do
    !$omp end do nowait
    
    !$omp end parallel
    
  end procedure init_eq_all_sub

end submodule equations_all