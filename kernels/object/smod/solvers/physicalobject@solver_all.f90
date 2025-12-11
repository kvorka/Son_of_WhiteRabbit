submodule (physicalobject) solver_all
  implicit none ; contains
  
  module procedure solve_all_sub
    integer :: ij, ijm, ir, is
    
    !$omp parallel private (ir,is,ij)
    
    !$omp do
    do ijm = 1, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rtemp(ir,ijm) = this%rtemp(ir,ijm) + this%mat%temp(ij)%multipl_fn(2*(ir-1)+1, this%sol%temp(:,ijm))
      end do
      
      do ir = 1, this%nd
        is = 2*(ir-1)+1
        
        this%sol%temp(is  ,ijm) = this%rtemp(ir,ijm)
        this%sol%temp(is+1,ijm) = czero
      end do
      
      ir = this%nd+1
        this%sol%temp(2*this%nd+1,ijm) = this%rtemp(ir,ijm)
      
      call this%mat%temp(ij)%luSolve_sub( this%sol%temp(:,ijm) )
    end do
    !$omp end do nowait
    
    !$omp do
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
    !$omp end do nowait
    
    !$omp do
    do ijm = 2, this%jms
      ij = this%j_indx(ijm)
      
      do ir = 2, this%nd
        this%rsph1(ir,ijm) = this%rsph1(ir,ijm) + this%mat%mech(ij)%multipl_fn(5*(ir-1)+1,this%sol%mech(:,ijm))
        this%rsph2(ir,ijm) = this%rsph2(ir,ijm) + this%mat%mech(ij)%multipl_fn(5*(ir-1)+2,this%sol%mech(:,ijm))
      end do
      
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
    !$omp end do nowait
    
    !$omp end parallel
      
  end procedure solve_all_sub
  
end submodule solver_all