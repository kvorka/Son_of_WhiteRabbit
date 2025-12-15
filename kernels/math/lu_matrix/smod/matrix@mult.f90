submodule (matrix) mult
  implicit none; contains
  
  module procedure matrix_multiple_sub
    integer :: k, indstart, indend, i1, i2
    
    k        = i-this%ld-1
    indstart = max(1,1-k)
    indend   = min(this%ldu,this%n-k)
    
    do i2 = indstart, indend
      do i1 = 0, howmany
        bout(i1) = bout(i1) + this%M(i2,i) * bin(i1,k+i2)
      end do
    end do
    
  end procedure matrix_multiple_sub
  
end submodule mult