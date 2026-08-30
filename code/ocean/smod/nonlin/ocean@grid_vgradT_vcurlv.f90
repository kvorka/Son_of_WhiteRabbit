submodule (ocean) grid_vgradT_vcurlv
  implicit none; contains
  
  module procedure grid_op_vgradT_vcurlv_sub
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
    
  end procedure grid_op_vgradT_vcurlv_sub
  
end submodule grid_vgradT_vcurlv