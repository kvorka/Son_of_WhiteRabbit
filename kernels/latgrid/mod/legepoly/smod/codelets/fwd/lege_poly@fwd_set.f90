submodule (lege_poly) fwd_set
  implicit none; contains
  
  module procedure fwd_set_sub
    integer        :: i0, i3
    real(kind=dbl) :: cr1, cr2, cr3, cr4, p1, p2, p3, p4
    
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
      cr1 = zero
      cr2 = zero
      cr3 = zero
      cr4 = zero
      
      !$omp simd reduction (+:cr1,cr2,cr3,cr4)
      do i0 = 1, ndbl
        p1 = pmj(i0,1)
        p2 = pmj(i0,2)
        p3 = pmj(i0,3)
        p4 = pmj(i0,4)
        
        cr1 = cr1 + p1 * swork(i0,1,1,i3) + p2 * swork(i0,2,1,i3) + p3 * swork(i0,3,1,i3) + p4 * swork(i0,4,1,i3)
        cr2 = cr2 + p1 * swork(i0,1,2,i3) + p2 * swork(i0,2,2,i3) + p3 * swork(i0,3,2,i3) + p4 * swork(i0,4,2,i3)
        cr3 = cr3 + p1 * swork(i0,1,3,i3) + p2 * swork(i0,2,3,i3) + p3 * swork(i0,3,3,i3) + p4 * swork(i0,4,3,i3)
        cr4 = cr4 + p1 * swork(i0,1,4,i3) + p2 * swork(i0,2,4,i3) + p3 * swork(i0,3,4,i3) + p4 * swork(i0,4,4,i3)
      end do
      
      cr(1,i3) = cr(1,i3) + cr1
      cr(2,i3) = cr(2,i3) + cr2
      cr(3,i3) = cr(3,i3) + cr3
      cr(4,i3) = cr(4,i3) + cr4
    end do
    
  end procedure fwd_set_sub
  
end submodule fwd_set