submodule (physicalobject) solver_all
  implicit none ; contains
  
  module procedure solve_all_sub
    integer :: ij, im, ir, is
    
    !$omp parallel private (ir,im,is)
    
    !$omp do schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        call this%mat%temp(ij)%addmultipl_sub( 2*(ir-1)+1, ij, this%sol%temp(ij)%arr, this%rhs%temp(ij)%arr(0,ir) )
      end do
      
      do ir = 1, this%nd
        is = 2*(ir-1)+1
        
        do im = 0, ij
          this%sol%temp(ij)%arr(im,is  ) = this%rhs%temp(ij)%arr(im,ir)
          this%sol%temp(ij)%arr(im,is+1) = czero
        end do
      end do
      
      ir = this%nd+1
        do im = 0, ij
          this%sol%temp(ij)%arr(im,2*this%nd+1) = this%rhs%temp(ij)%arr(im,ir)
        end do
      
      call this%mat%temp(ij)%luSolve_sub( ij, this%sol%temp(ij)%arr )
    end do
    !$omp end do nowait
    
    !$omp do schedule (guided,2)
    do ij = 1, this%jmax
      do ir = 2, this%nd
        call this%mat%torr(ij)%addmultipl_sub( 2*(ir-1)+1, ij, this%sol%torr(ij)%arr, this%rhs%torr(ij)%arr(0,ir) )
      end do
      
      do ir = 1, this%nd
        is = 2*(ir-1)+1
        
        do im = 0, ij
          this%sol%torr(ij)%arr(im,is  ) = this%rhs%torr(ij)%arr(im,ir)
          this%sol%torr(ij)%arr(im,is+1) = czero
        end do
      end do
      
      ir = this%nd+1
        do im = 0, ij
          this%sol%torr(ij)%arr(im,2*this%nd+1) = this%rhs%torr(ij)%arr(im,ir)
        end do
      
      call this%mat%torr(ij)%luSolve_sub( ij, this%sol%torr(ij)%arr )
    end do
    !$omp end do nowait
    
    !$omp do schedule (guided,2)
    do ij = 1, this%jmax
      do ir = 2, this%nd
        call this%mat%mech(ij)%addmultipl_sub( 5*(ir-1)+1, ij, this%sol%mech(ij)%arr, this%rhs%sph1(ij)%arr(0,ir) )
        call this%mat%mech(ij)%addmultipl_sub( 5*(ir-1)+2, ij, this%sol%mech(ij)%arr, this%rhs%sph2(ij)%arr(0,ir) )
      end do
      
      do ir = 1, this%nd
        is = 5*(ir-1)+1
        
        do im = 0, ij
          this%sol%mech(ij)%arr(im,is  ) = this%rhs%sph1(ij)%arr(im,ir)
          this%sol%mech(ij)%arr(im,is+1) = this%rhs%sph2(ij)%arr(im,ir)
          this%sol%mech(ij)%arr(im,is+2) = czero
          this%sol%mech(ij)%arr(im,is+3) = czero
          this%sol%mech(ij)%arr(im,is+4) = czero
        end do
      end do
        
      ir = this%nd+1
        do im = 0, ij
          this%sol%mech(ij)%arr(im,5*this%nd+1) = this%rhs%sph1(ij)%arr(im,ir)
          this%sol%mech(ij)%arr(im,5*this%nd+2) = this%rhs%sph2(ij)%arr(im,ir)
        end do
      
      call this%mat%mech(ij)%luSolve_sub( ij, this%sol%mech(ij)%arr )
    end do
    !$omp end do nowait
    
    !$omp end parallel
      
  end procedure solve_all_sub
  
end submodule solver_all