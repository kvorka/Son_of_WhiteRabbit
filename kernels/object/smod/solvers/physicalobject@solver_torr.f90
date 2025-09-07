submodule (physicalobject) solver_torr
  implicit none ; contains
  
  module procedure solve_torr_sub
    integer :: ij, ijm, ir, is
    
    !$omp parallel do private (ir,is,ij)
    do ijm = 2, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rtorr(ir,ijm) = this%rtorr(ir,ijm) + this%mat%torr(ij)%multipl_fn(2*(ir-1)+1,this%sol%torr(:,ijm))
      end do
      
      do ir = 1, this%nd
        is = 2*(ir-1) + 1
        
        this%sol%torr(is  ,ijm) = this%rtorr(ir,ijm)
        this%sol%torr(is+1,ijm) = czero
      end do
      
      ir = this%nd+1
        this%sol%torr(2*this%nd+1,ijm) = this%rtorr(ir,ijm)
        
      call this%mat%torr(ij)%luSolve_sub( this%sol%torr(:,ijm) )
    end do
    !$omp end parallel do
    
  end procedure solve_torr_sub
  
end submodule solver_torr