submodule (physicalobject) solver_mech
  implicit none ; contains
  
  module procedure solve_mech_ij_sub
    integer :: ir, is
    
    do ir = 2, this%nd
      call this%mech(ij)%addmultipl1_sub( 5*(ir-1)+1, ir )
      call this%mech(ij)%addmultipl2_sub( 5*(ir-1)+2, ir )
    end do
    
    do ir = 1, this%nd
      is = 5*(ir-1)+1
      
      call copy_carray_sub( this%mech(ij)%mm1, this%mech(ij)%rhs1(0,ir), this%mech(ij)%sol(0,is  ) )
      call copy_carray_sub( this%mech(ij)%mm1, this%mech(ij)%rhs2(0,ir), this%mech(ij)%sol(0,is+1) )
      
      call zero_carray_sub( this%mech(ij)%mm1, this%mech(ij)%sol(0,is+2) )
      call zero_carray_sub( this%mech(ij)%mm1, this%mech(ij)%sol(0,is+3) )
      call zero_carray_sub( this%mech(ij)%mm1, this%mech(ij)%sol(0,is+4) )
    end do
    
    call copy_carray_sub( this%mech(ij)%mm1, this%mech(ij)%rhs1(0,this%nd+1), this%mech(ij)%sol(0,5*this%nd+1) )
    call copy_carray_sub( this%mech(ij)%mm1, this%mech(ij)%rhs2(0,this%nd+1), this%mech(ij)%sol(0,5*this%nd+2) )
    
    call this%mech(ij)%luSolve_sub()
    
  end procedure solve_mech_ij_sub
  
end submodule solver_mech