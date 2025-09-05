submodule (physicalobject) solver_temp
  implicit none ; contains
  
  module procedure solve_temp_sub
    integer :: ij, ir, is, ijm
    
    !$omp parallel do private (ir,is,ij)
    do ijm = 1, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = this%rtemp(ir,ijm) + this%mat%temp(ij)%multipl_fn(2*(ir-1)+1, this%sol%temp(:,ijm))
      end do
      
      !$omp simd private (is)
      do ir = 1, this%nd
        is = 2*(ir-1)+1
        
        this%sol%temp(is  ,ijm) = this%rtemp(ir,ijm)
        this%sol%temp(is+1,ijm) = czero
      end do
      
      ir = this%nd+1
        this%sol%temp(2*this%nd+1,ijm) = this%rtemp(ir,ijm)
      
      call this%mat%temp(ij)%luSolve_sub( this%sol%temp(:,ijm) )
    end do
    !$omp end parallel do
    
  end procedure solve_temp_sub
  
end submodule solver_temp
