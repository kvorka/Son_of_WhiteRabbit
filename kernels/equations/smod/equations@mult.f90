submodule (equations) mult
  implicit none; contains
  
  module procedure matrix_multiple_sub
    integer :: k, i1, i2
    
    k = i-this%ld-1
    
    do i2 = max(1,1-k), min(this%ldu,this%n-k)
      do concurrent ( i1 = 0:this%mm )
        bout(i1) = bout(i1) + this%M(i2,i) * this%sol(i1,k+i2)
      end do
    end do
    
  end procedure matrix_multiple_sub
  
end submodule mult