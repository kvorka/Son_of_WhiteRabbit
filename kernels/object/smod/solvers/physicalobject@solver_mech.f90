submodule (physicalobject) solver_mech
  implicit none ; contains
  
  module procedure solve_mech_sub
    integer :: ij, ijm, ir, is
    
    !$omp parallel do private (ir,is,ij)
    do ijm = 2, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rsph1(ir,ijm) = this%rsph1(ir,ijm) + this%mat%mech(ij)%multipl_fn(5*(ir-1)+1,this%sol%mech(:,ijm))
        this%rsph2(ir,ijm) = this%rsph2(ir,ijm) + this%mat%mech(ij)%multipl_fn(5*(ir-1)+2,this%sol%mech(:,ijm))
      end do
      
      !$omp simd private (is)
      do ir = 1, this%nd
        is = 5*(ir-1)+1
        
        this%sol%mech(is  ,ijm) = this%rsph1(ir,ijm)
        this%sol%mech(is+1,ijm) = this%rsph2(ir,ijm)
        this%sol%mech(is+2,ijm) = czero
        this%sol%mech(is+3,ijm) = czero
        this%sol%mech(is+4,ijm) = czero
      end do
        
      ir = this%nd+1
        this%sol%mech(5*this%nd+1,ijm) = this%rsph1(ir,ijm)
        this%sol%mech(5*this%nd+2,ijm) = this%rsph2(ir,ijm)
        
      call this%mat%mech(ij)%luSolve_sub( this%sol%mech(:,ijm) )
    end do
    !$omp end parallel do
      
  end procedure solve_mech_sub
  
end submodule solver_mech