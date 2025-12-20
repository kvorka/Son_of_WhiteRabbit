submodule (equations) mult
  implicit none; contains
  
  module procedure matrix_multiple_sub
    integer :: k, j
    
    k = i-this%ld-1
    
    do j = max(1,1-k), min(this%ldu,this%n-k)
      call copy3_carray_sub( this%mm+1, this%M(j,i), this%sol(0,k+j), bout )
    end do
    
  end procedure matrix_multiple_sub
  
end submodule mult