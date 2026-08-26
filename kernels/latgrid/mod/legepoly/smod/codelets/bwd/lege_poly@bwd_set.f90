submodule (lege_poly) bwd_set
  implicit none; contains
  
  module procedure bwd_set_sub
    integer :: i0, i2, i3
    
    select case (ma1)
      case (1)
        !$omp simd
        do i0 = 1, ndbl
          pmm(i0,1) = cff
          pmm(i0,2) = cff
          pmm(i0,3) = cff
          pmm(i0,4) = cff
        end do
      
      case default
        !$omp simd
        do i0 = 1, ndbl
          pmm(i0,1) = cff * sinx(i0,1) * pmm(i0,1)
          pmm(i0,2) = cff * sinx(i0,2) * pmm(i0,2)
          pmm(i0,3) = cff * sinx(i0,3) * pmm(i0,3)
          pmm(i0,4) = cff * sinx(i0,4) * pmm(i0,4)
        end do
    end select
    
    !$omp simd
    do i0 = 1, ndbl
      pmj1(i0,1) = zero
      pmj1(i0,2) = zero
      pmj1(i0,3) = zero
      pmj1(i0,4) = zero
      
      pmj(i0,1)  = pmm(i0,1) / cosx(i0,1)
      pmj(i0,2)  = pmm(i0,2) / cosx(i0,2)
      pmj(i0,3)  = pmm(i0,3) / cosx(i0,3)
      pmj(i0,4)  = pmm(i0,4) / cosx(i0,4)
    end do
    
    do i3 = 1, n
      do i2 = 1, 4
        !$omp simd
        do i0 = 1, ndbl
          swork(i0,1,i2,i3) = pmj(i0,1) * cc(i2,i3)
          swork(i0,2,i2,i3) = pmj(i0,2) * cc(i2,i3)
          swork(i0,3,i2,i3) = pmj(i0,3) * cc(i2,i3)
          swork(i0,4,i2,i3) = pmj(i0,4) * cc(i2,i3)
        end do
      end do
    end do
    
  end procedure bwd_set_sub
  
end submodule bwd_set