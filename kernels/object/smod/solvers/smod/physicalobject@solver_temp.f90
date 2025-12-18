submodule (physicalobject) solver_temp
  implicit none ; contains
  
  module procedure solve_temp_ij_sub
    integer :: ir, is, im
    
    do concurrent ( ir = 2:this%nd )
      call this%temp(ij)%addmultipl_sub( 2*(ir-1)+1, this%temp(ij)%rhs1(0,ir) )
    end do
    
    do ir = 1, this%nd
      is = 2*(ir-1)+1
      
      do concurrent ( im = 0:ij )
        this%temp(ij)%sol(im,is  ) = this%temp(ij)%rhs1(im,ir)
        this%temp(ij)%sol(im,is+1) = czero
      end do
    end do
    
    ir = this%nd+1
      do concurrent ( im = 0:ij )
        this%temp(ij)%sol(im,2*this%nd+1) = this%temp(ij)%rhs1(im,ir)
      end do
    
    call this%temp(ij)%luSolve_sub()
    
  end procedure solve_temp_ij_sub
  
end submodule solver_temp
