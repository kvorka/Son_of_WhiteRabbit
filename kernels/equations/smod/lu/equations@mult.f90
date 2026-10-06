submodule (equations) mult
  implicit none; contains
  
  module procedure matrix_multiple1_sub
    integer :: i, k, k1
    
    k  = isol-this%ld-1
    k1 = max(0,-k)
    
    do i = k1+1, min(this%ldu,this%n-k)
      call copy3_carray_sub( this%mm1, this%M(i,isol), this%sol(0,i+k), this%rhs1(0,irhs) )
    end do
    
  end procedure matrix_multiple1_sub
  
  module procedure matrix_multiple2_sub
    integer :: i, k, k1
    
    k  = isol-this%ld-1
    k1 = max(0,-k)
    
    do i = k1+1, min(this%ldu,this%n-k)
      call copy3_carray_sub( this%mm1, this%M(i,isol), this%sol(0,i+k), this%rhs2(0,irhs) )
    end do
    
  end procedure matrix_multiple2_sub
  
end submodule mult