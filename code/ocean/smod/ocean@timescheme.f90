submodule (ocean) timescheme
  implicit none; contains
  
  module subroutine grid_op_scvv_vcvxv_sub(nfour, grid, gtmp)
    integer,        intent(in)    :: nfour
    real(kind=dbl), intent(inout) :: grid(ndbl,4,0:*)
    real(kind=dbl), intent(out)   :: gtmp(ndbl,4,0:*)
    integer                       :: i0, i1, i3
    
    do i3 = 0, nfour-1
      call gcopy_sub( 9, grid(1,1,9*i3), gtmp )
      
      do i1 = 1, 4
        !$omp simd
        do i0 = 1, ndbl
          grid(i0,i1,0+4*i3) = gtmp(i0,i1,0) * gtmp(i0,i1,3) + gtmp(i0,i1,1) * gtmp(i0,i1,4) + gtmp(i0,i1,2) * gtmp(i0,i1,5)
          grid(i0,i1,1+4*i3) = gtmp(i0,i1,2) * gtmp(i0,i1,7) - gtmp(i0,i1,1) * gtmp(i0,i1,8)
          grid(i0,i1,2+4*i3) = gtmp(i0,i1,0) * gtmp(i0,i1,8) - gtmp(i0,i1,2) * gtmp(i0,i1,6)
          grid(i0,i1,3+4*i3) = gtmp(i0,i1,1) * gtmp(i0,i1,6) - gtmp(i0,i1,0) * gtmp(i0,i1,7)
        end do
      end do
    end do
    
  end subroutine grid_op_scvv_vcvxv_sub
  
  module procedure time_scheme_ocean_sub
    integer                                :: ik, ir, ij, ij0
    type(c_ptr)                            :: c_tWork
    real(kind=dbl),    allocatable         :: rccWork(:), rcrWork(:)
    real(kind=dbl),    pointer, contiguous :: tWork(:)
    complex(kind=dbl), allocatable, target :: vWork(:)
    complex(kind=dbl), pointer, contiguous :: T(:), gradT(:), v(:), curlv(:), work1(:), work2(:), work3(:)
    
    !!******************************************************************************************************!!
    !!** Move time by one time-step.                                                                      **!!
    !!******************************************************************************************************!!
    this%t = this%t + this%dt
    
    !!******************************************************************************************************!!
    !!** Unaligned memory allocations are handled before parallel region. Namely, vWork holds spectral    **!!
    !!** coefficients of v, T, curlv and gradT, while it is also being used for complex transpositions.   **!!
    !!** rccWork and rcrWork are just temporal holders for shuffled real/imag south/north thingy in the   **!!
    !!** spectral transform. The sizes are set according to number of backword transforms, nb = 9 requi-  **!!
    !!** res working array, which is 2*9+1 rxd%jms1 sized arrays. With nb=9 and nf=4 we set also rcc/rcr. **!!
    !!******************************************************************************************************!!
    call this%rxd%alloc_work_rxd_sub( 19, vWork )
    call this%lat_grid%lgp%alloc_rscal_sub( 9, rccWork )
    call this%lat_grid%lgp%alloc_rscal_sub( 4, rcrWork )
    
    !$omp parallel private (c_tWork, tWork, rccWork, rcrWork, vWork, T, v, gradT, curlv, work1, work2, work3)
    
    !!******************************************************************************************************!!
    !!** At first, copy the non-linear terms from previous time-step into right-hand sides. These are re- **!!
    !!** scaled by a corresponding Adams-Bashforth time-step coefficient.                                 **!!
    !!******************************************************************************************************!!
    !$omp do private (ir,ij0) schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        call copy2_carray_sub( ij+1, 1-this%ab, this%ntemp(ij0,ir), this%temp(ij)%rhs1(0,ir) )
        call copy2_carray_sub( ij+1, 1-this%ab, this%ntorr(ij0,ir), this%torr(ij)%rhs1(0,ir) )
        call copy2_carray_sub( ij+1, 1-this%ab, this%nsph1(ij0,ir), this%mech(ij)%rhs1(0,ir) )
        call copy2_carray_sub( ij+1, 1-this%ab, this%nsph2(ij0,ir), this%mech(ij)%rhs2(0,ir) )
      end do
    end do
    !$omp end do
    
    !!******************************************************************************************************!!
    !!** Aligned memory allocations for transform guided by high number of backword transforms.           **!!
    !!******************************************************************************************************!!
    call this%lat_grid%alloc_work_lgrid_sub( 9, c_tWork, tWork )
    
    !!******************************************************************************************************!!
    !!** Pointers set-up for cleaner code.                                                                **!!
    !!******************************************************************************************************!!
    T     => vWork( 1                :  1 * this%jms )
    v     => vWork( 1 + 1 * this%jms :  4 * this%jms )
    gradT => vWork( 1 + 4 * this%jms :  7 * this%jms )
    curlv => vWork( 1 + 7 * this%jms : 10 * this%jms )
    
    work1 => vWork( 1 +  1 * this%rxd%jms1 : 10 * this%rxd%jms1 )
    work2 => vWork( 1 + 10 * this%rxd%jms1 : 19 * this%rxd%jms1 )
    work3 => vWork( 1 +  2 * this%rxd%jms1 :  5 * this%rxd%jms1 )
    
    !!******************************************************************************************************!!
    !!** Main radial loop computing the non-linear terms for this time-step.                              **!!
    !!******************************************************************************************************!!
    !$omp do
    do ir = 2, this%nd
      !! Get vorticity and temperature gradient with scaling factors
      call this%curlv_ptp_rr_jm_sub( ir, v, curlv, 1/this%Pr, work2 )
      call this%gradT_ptp_rr_jm_sub( ir, T, gradT, -1._dbl,   work2 )
      
      !! Add ez for Coriolis force into curlv
      curlv(2)%re = curlv(2)%re + s4pi * ( 2 / this%Ek )
      
      !! Transpose and shuffle the data into contiguous storage, meaning from
      !! v(l-1), v(l), v(l+1), q(l-1), q(l), ... into v(l-1), q(l-1), curlv(l-1), ...
      call trshf_3_carray_sub( length = this%jms, &
                               v1     = v,        &
                               v2     = gradT,    &
                               v3     = curlv,    &
                               ca     = work2     )
      
      !! Transform the 3 vectors into 9 scalars, layout is vx, vy, vz, qx, qy, ...
      call this%rxd%vec2scal_jm_to_mj_sub( nca = 3,     &
                                           ca  = work2, &
                                           cc  = work1  )
      
      !! After all the preparation, the transform is here.
      call this%lat_grid%transform_sub( nf    = 4,                     &
                                        nb    = 9,                     &
                                        cc    = work1,                 &
                                        cr    = work2,                 &
                                        rcc   = rccWork,               &
                                        rcr   = rcrWork,               &
                                        work  = tWork,                 &
                                        g_sub = grid_op_scvv_vcvxv_sub )
      
      !! Another layer of transposing: from (4,mj) to (mj,4)
      call trans_4_carray_sub( length   = this%rxd%jms1, &
                               arr_from = work2,         &
                               arr_to   = work1          )
      
      !! Copy and shuffle the data for vgradT into ntemp.
      call this%rxd%scal2scal_mj_to_jm_sub( cr  = work1,           &
                                            cjm = this%ntemp(1,ir) )
      
      !! Copy and shuffle the data for curlv into nsph1, ntorr, nsph2.
      call this%rxd%scal2vec_mj_to_jm_sub( cr   = work3,            &
                                           cjm1 = this%nsph1(1,ir), &
                                           cjm2 = this%ntorr(1,ir), &
                                           cjm3 = this%nsph2(1,ir)  )
      
      !! Add the thermal buoyancy force with Newtonian gravity profile.
      call this%buoy_rr_jml_sub( fac  = this%Ra / ( 1 - this%r_ud )**2 / this%rad_grid%rr(ir)**2, &
                                 src  = T,                &
                                 pol1 = this%nsph1(1,ir), &
                                 pol2 = this%nsph2(1,ir)  )
    end do
    !$omp end do
    
    !!******************************************************************************************************!!
    !!** Clean aligned memory after transform.                                                            **!!
    !!******************************************************************************************************!!
    call free_aligned_sub( c_tWork, tWork )
    
    !!******************************************************************************************************!!
    !!** Add the non-linear terms computed in this time-step to the right-hand side. The non-linear terms **!!
    !!** are weighted by the appropriate Addams-Bashforth factor.                                         **!!
    !!******************************************************************************************************!!
    !$omp do private (ir,ij0) schedule (guided,2)
    do ij = 0, this%jmax
      do ir = 2, this%nd
        ij0 = jm(ij,0)
        
        call copy3_carray_sub( ij+1, this%ab, this%ntemp(ij0,ir), this%temp(ij)%rhs1(0,ir) )
        call copy3_carray_sub( ij+1, this%ab, this%ntorr(ij0,ir), this%torr(ij)%rhs1(0,ir) )
        call copy3_carray_sub( ij+1, this%ab, this%nsph1(ij0,ir), this%mech(ij)%rhs1(0,ir) )
        call copy3_carray_sub( ij+1, this%ab, this%nsph2(ij0,ir), this%mech(ij)%rhs2(0,ir) )
      end do
    end do
    !$omp end do
    
    !!******************************************************************************************************!!
    !!** Finally solve for the spectral coeffs.                                                           **!!
    !!******************************************************************************************************!!
    !$omp do
    do ik = 0, (this%jmax-1)/2
      call this%solve_temp_ij_sub( ik          )
      call this%solve_temp_ij_sub( this%jmax-ik)
      
      call this%solve_torr_ij_sub( ik           )
      call this%solve_torr_ij_sub( this%jmax-ik )
      
      call this%solve_mech_ij_sub( ik           )
      call this%solve_mech_ij_sub( this%jmax-ik )
    end do
    !$omp end do
    
    !$omp end parallel
    
    !!******************************************************************************************************!!
    !!** Clean memory after parallel region is done.                                                      **!!
    !!******************************************************************************************************!!
    deallocate( vWork, rccWork, rcrWork )
    
  end procedure time_scheme_ocean_sub
  
end submodule timescheme