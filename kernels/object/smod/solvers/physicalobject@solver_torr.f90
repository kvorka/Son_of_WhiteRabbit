submodule (physicalobject) solver_torr
  implicit none ; contains
  
  module procedure solve_torr_sub
    integer :: ij, im, ir, is
    
    !$omp parallel do private(im, ir, is) schedule (guided,2)
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
    !$omp end parallel do
    
  end procedure solve_torr_sub
  
end submodule solver_torr