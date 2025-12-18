submodule (physicalobject) equations_all
  implicit none ; contains
  
  module procedure init_eq_all_sub
    integer :: ik, ij
    
    allocate( this%temp(0:this%jmax) )
    allocate( this%torr(0:this%jmax) )
    allocate( this%mech(0:this%jmax) )
    
    !$omp parallel do private (ij)
    do ik = 0, (this%jmax-1)/2
      ij = ik
        call this%temp(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%torr(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%mech(ij)%init_sub( mm=ij, nvar=5*this%nd+2, nrhs=this%nd+1, ld=9, lu=8, def_rhs2=.true.  )
      
      ij = this%jmax-ik
        call this%temp(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%torr(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%mech(ij)%init_sub( mm=ij, nvar=5*this%nd+2, nrhs=this%nd+1, ld=9, lu=8, def_rhs2=.true.  )
    end do
    !$omp end parallel do
    
  end procedure init_eq_all_sub

end submodule equations_all