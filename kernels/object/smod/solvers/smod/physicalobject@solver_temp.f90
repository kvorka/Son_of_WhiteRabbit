submodule (physicalobject) solver_temp
  implicit none ; contains
  
  module procedure solve_temp_ij_sub
    integer :: ir, is
    
    do ir = 2, this%nd
      call this%temp(ij)%addmultipl_sub( 2*(ir-1)+1, this%temp(ij)%rhs1(0,ir) )
    end do
    
    do ir = 1, this%nd
      is = 2*(ir-1)+1
      
      call copy_carray_sub( ij+1, this%temp(ij)%rhs1(0,ir), this%temp(ij)%sol(0,is) )
      call zero_carray_sub( ij+1, this%temp(ij)%sol(0,is+1) )
    end do
    
    call copy_carray_sub( ij+1, this%temp(ij)%rhs1(0,this%nd+1), this%temp(ij)%sol(0,2*this%nd+1) )
    
    call this%temp(ij)%luSolve_sub()
    
  end procedure solve_temp_ij_sub
  
end submodule solver_temp
