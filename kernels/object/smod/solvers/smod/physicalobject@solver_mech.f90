submodule (physicalobject) solver_mech
  implicit none ; contains
  
  module procedure solve_mech_ij_sub
    integer :: im, ir, is
    
    do concurrent ( ir = 2:this%nd )
      call this%mech(ij)%addmultipl_sub( 5*(ir-1)+1, this%mech(ij)%rhs1(0,ir) )
      call this%mech(ij)%addmultipl_sub( 5*(ir-1)+2, this%mech(ij)%rhs2(0,ir) )
    end do
    
    do ir = 1, this%nd
      is = 5*(ir-1)+1
      
      do concurrent ( im = 0:ij )
        this%mech(ij)%sol(im,is  ) = this%mech(ij)%rhs1(im,ir)
        this%mech(ij)%sol(im,is+1) = this%mech(ij)%rhs2(im,ir)
        this%mech(ij)%sol(im,is+2) = czero
        this%mech(ij)%sol(im,is+3) = czero
        this%mech(ij)%sol(im,is+4) = czero
      end do
    end do
      
    ir = this%nd+1
      do concurrent ( im = 0:ij )
        this%mech(ij)%sol(im,5*this%nd+1) = this%mech(ij)%rhs1(im,ir)
        this%mech(ij)%sol(im,5*this%nd+2) = this%mech(ij)%rhs2(im,ir)
      end do
    
    call this%mech(ij)%luSolve_sub()
    
  end procedure solve_mech_ij_sub
  
end submodule solver_mech