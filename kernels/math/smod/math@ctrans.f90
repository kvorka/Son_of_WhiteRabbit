submodule (math) ctrans
  implicit none; contains
  
  module procedure trans_carray_sub
    integer :: i1, i2
    
    select case (n)
      case (4)
        !$omp simd
        do i1 = 1, length
          arr_to(i1,1) = arr_from(1,i1)
          arr_to(i1,2) = arr_from(2,i1)
          arr_to(i1,3) = arr_from(3,i1)
          arr_to(i1,4) = arr_from(4,i1)
        end do
      
      case default
        do i2 = 1, n
          !$omp simd
          do i1 = 1, length
            arr_to(i1,i2) = arr_from(i2,i1)
          end do
        end do
    end select
    
  end procedure trans_carray_sub
  
end submodule ctrans