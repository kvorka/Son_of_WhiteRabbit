submodule (lege_poly) bwd_set
  implicit none; contains
  
  module procedure bwd_set_sub
    integer        :: i0, i3
    real(kind=dbl) :: p1, p2, p3, p4, cc1, cc2, cc3, cc4
    
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
      cc1 = cc(1,i3)
      cc2 = cc(2,i3)
      cc3 = cc(3,i3)
      cc4 = cc(4,i3)
      
      !$omp simd
      do i0 = 1, ndbl
        p1 = pmj(i0,1)
        p2 = pmj(i0,2)
        p3 = pmj(i0,3)
        p4 = pmj(i0,4)
        
        swork(i0,1,1,i3) = p1 * cc1
        swork(i0,2,1,i3) = p2 * cc1
        swork(i0,3,1,i3) = p3 * cc1
        swork(i0,4,1,i3) = p4 * cc1
        swork(i0,1,2,i3) = p1 * cc2
        swork(i0,2,2,i3) = p2 * cc2
        swork(i0,3,2,i3) = p3 * cc2
        swork(i0,4,2,i3) = p4 * cc2
        swork(i0,1,3,i3) = p1 * cc3
        swork(i0,2,3,i3) = p2 * cc3
        swork(i0,3,3,i3) = p3 * cc3
        swork(i0,4,3,i3) = p4 * cc3
        swork(i0,1,4,i3) = p1 * cc4
        swork(i0,2,4,i3) = p2 * cc4
        swork(i0,3,4,i3) = p3 * cc4
        swork(i0,4,4,i3) = p4 * cc4
      end do
    end do
    
  end procedure bwd_set_sub
  
end submodule bwd_set