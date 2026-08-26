submodule (equations) lu_solve
  implicit none; contains
  
  module procedure lu_solve_sub
    integer :: i, j
    
    do i = 1, this%n-1
      j = this%I(i)
      
      if ( j /= i ) then
        call swap_carray_sub( this%mm1, this%sol(0,i), this%sol(0,j) )
      end if
      
      do j = 1, min(this%n-i,this%ld)
        call copy3_carray_sub( this%mm1, -this%L(j,i), this%sol(0,i), this%sol(0,i+j) )
      end do
    end do
    
    i = this%n
      j = this%I(i)
      
      if ( j /= i ) then
        call swap_carray_sub( this%mm1, this%sol(0,i), this%sol(0,j) )
      end if
      
      call copy1_carray_sub( this%mm1, 1/this%U(1,i), this%sol(0,i) )
      
    do i = this%n-1, 1, -1
      do j = 1, min(this%ldu-1,this%n-i)
        call copy3_carray_sub( this%mm1, -this%U(j+1,i), this%sol(0,i+j), this%sol(0,i) )
      end do
      
      call copy1_carray_sub( this%mm1, 1/this%U(1,i), this%sol(0,i) )
    end do
    
  end procedure lu_solve_sub
  
end submodule lu_solve
