submodule (physicalobject) solver_mech
  implicit none ; contains
  
  module procedure solve_mech_sub
    integer :: ij, im, ir, is
    
    !$omp parallel do private(ir, is, im) schedule (guided,2)
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
    !$omp end parallel do
      
  end procedure solve_mech_sub
  
end submodule solver_mech