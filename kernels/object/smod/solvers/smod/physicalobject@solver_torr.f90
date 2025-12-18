submodule (physicalobject) solver_torr
  implicit none ; contains
  
  module procedure solve_torr_ij_sub
    integer :: im, ir, is
    
    do concurrent ( ir = 2:this%nd )
      call this%torr(ij)%addmultipl_sub( 2*(ir-1)+1, this%torr(ij)%rhs1(0,ir) )
    end do
    
    do ir = 1, this%nd
      is = 2*(ir-1)+1
      
      do concurrent ( im = 0:ij )
        this%torr(ij)%sol(im,is  ) = this%torr(ij)%rhs1(im,ir)
        this%torr(ij)%sol(im,is+1) = czero
      end do
    end do
    
    ir = this%nd+1
      do concurrent ( im = 0:ij )
        this%torr(ij)%sol(im,2*this%nd+1) = this%torr(ij)%rhs1(im,ir)
      end do
    
    call this%torr(ij)%luSolve_sub()
    
  end procedure solve_torr_ij_sub
  
end submodule solver_torr