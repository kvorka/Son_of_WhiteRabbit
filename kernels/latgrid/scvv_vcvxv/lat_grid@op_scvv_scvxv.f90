submodule (lateral_grid) op_scvv_vcvxv
  implicit none; contains
  
  module procedure grid_op_scvv_vcvxv_sub
    integer :: i0, i1, i3
    
    do i3 = 0, nfour-1
      call gcopy_sub( 9, grid(1,1,9*i3), gtmp )
      
      do i1 = 1, 4
        !$omp simd
        do i0 = 1, ndbl
          grid(i0,i1,0+4*i3) = gtmp(i0,i1,0) * gtmp(i0,i1,3) + gtmp(i0,i1,1) * gtmp(i0,i1,4) + gtmp(i0,i1,2) * gtmp(i0,i1,5)
          grid(i0,i1,1+4*i3) = gtmp(i0,i1,2) * gtmp(i0,i1,7) - gtmp(i0,i1,1) * gtmp(i0,i1,8)
          grid(i0,i1,2+4*i3) = gtmp(i0,i1,0) * gtmp(i0,i1,8) - gtmp(i0,i1,2) * gtmp(i0,i1,6)
          grid(i0,i1,3+4*i3) = gtmp(i0,i1,1) * gtmp(i0,i1,6) - gtmp(i0,i1,0) * gtmp(i0,i1,7)
        end do
      end do
    end do
    
  end procedure grid_op_scvv_vcvxv_sub
  
end submodule op_scvv_vcvxv