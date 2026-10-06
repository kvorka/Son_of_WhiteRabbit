submodule (gridsubs) scvv_vcvxv
  implicit none; contains
  
  module procedure grid_op_scvv_vcvxv_sub
    integer :: i3
    
    do i3 = 0, nfour-1
      call scvv_vcvxv_sub( grid(36*ndbl*i3), grid(16*ndbl*i3), gtmp )
    end do
    
  end procedure grid_op_scvv_vcvxv_sub
  
end submodule scvv_vcvxv