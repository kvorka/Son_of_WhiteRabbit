submodule (matrix) lu_solve
  implicit none; contains
  
  module procedure lu_solve_sub
    integer                        :: j, i1, i2
    complex(kind=dbl), allocatable :: dum(:)
    
    allocate( dum(0:howmany) )
    
    do j = 1, this%n
      i2 = this%I(j)
      
      do i1 = 0, howmany
        dum(i1) = b(i1,i2)
      end do
      
      if (i2 /= j) then
        do i1 = 0, howmany
          b(i1,i2) = b(i1,j)
          b(i1,j)  = dum(i1)
        end do
      end if
      
      do i2 = j+1, min(this%n,this%ld+j)
        do i1 = 0, howmany
          b(i1,i2) = b(i1,i2) - this%L(i2-j,j) * dum(i1)
        end do
      end do
    end do
    
    do i2 = this%n, 1, -1
      do i1 = 0, howmany
        dum(i1) = b(i1,i2)
      end do

      do j = 2, min(this%ldu,this%n-i2+1)
        do i1 = 0, howmany
          dum(i1) = dum(i1) - this%U(j,i2) * b(i1,i2+j-1)
        end do
      end do
      
      do i1 = 0, howmany
        b(i1,i2) = dum(i1) / this%U(1,i2)
      end do
    end do
    
    deallocate( dum )
    
  end procedure lu_solve_sub
  
end submodule lu_solve
