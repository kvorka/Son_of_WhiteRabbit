submodule (gridsubs) scvv_vcvxv
  implicit none; contains
  
  module procedure grid_op_scvv_vcvxv_sub
    integer :: i3
    
    do i3 = 0, nfour-1
      call gcopy_sub( 9, grid(1,9*i3), gtmp )
      call scvv_vcvxv_sub( gtmp, grid(1,4*i3) )
    end do
    
  end procedure grid_op_scvv_vcvxv_sub
  
end submodule scvv_vcvxv