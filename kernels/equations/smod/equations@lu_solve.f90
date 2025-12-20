submodule (equations) lu_solve
  implicit none; contains
  
  module procedure lu_solve_sub
    integer                        :: j, i
    complex(kind=dbl), allocatable :: dum(:)
    
    allocate( dum(0:this%mm) )
    
    do j = 1, this%n
      i = this%I(j)
      
      call copy_carray_sub( this%mm+1, this%sol(0,i), dum )
      
      if ( i /= j ) then
        call copy_carray_sub( this%mm+1, this%sol(0,j), this%sol(0,i) )
        call copy_carray_sub( this%mm+1, dum, this%sol(0,j) )
      end if
      
      do i = j+1, min(this%n,this%ld+j)
        call copy3_carray_sub( this%mm+1, -this%L(i-j,j), dum, this%sol(0,i) )
      end do
    end do
    
    do i = this%n, 1, -1
      call copy_carray_sub( this%mm+1, this%sol(0,i), dum )

      do j = 2, min(this%ldu,this%n-i+1)
        call copy3_carray_sub( this%mm+1, -this%U(j,i), this%sol(0,i+j-1), dum )
      end do
      
      call copy2_carray_sub( this%mm+1, 1/this%U(1,i), dum, this%sol(0,i) )
    end do
    
    deallocate( dum )
    
  end procedure lu_solve_sub
  
end submodule lu_solve
