submodule (physicalobject) solver_temp
  implicit none ; contains
  
  module procedure solve_temp_ij_sub
    integer :: ir, is, im
    
    do concurrent ( ir = 2:this%nd )
      call this%mat%temp(ij)%addmultipl_sub( 2*(ir-1)+1, ij, this%sol%temp(ij)%arr, this%rhs%temp(ij)%arr(0,ir) )
    end do
    
    do ir = 1, this%nd
      is = 2*(ir-1)+1
      
      do concurrent ( im = 0:ij )
        this%sol%temp(ij)%arr(im,is  ) = this%rhs%temp(ij)%arr(im,ir)
        this%sol%temp(ij)%arr(im,is+1) = czero
      end do
    end do
    
    ir = this%nd+1
      do concurrent ( im = 0:ij )
        this%sol%temp(ij)%arr(im,2*this%nd+1) = this%rhs%temp(ij)%arr(im,ir)
      end do
    
    call this%mat%temp(ij)%luSolve_sub( ij, this%sol%temp(ij)%arr )
    
  end procedure solve_temp_ij_sub
  
end submodule solver_temp
