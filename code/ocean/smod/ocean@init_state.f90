submodule (ocean) init_state
  implicit none; contains
  
  subroutine init_conduction_sub(this)
    class(T_ocean), intent(inout) :: this
    integer                       :: is, ir, ij, im
    real(kind=dbl)                :: realp, imagp, dt_help, ab_help, cf_help
    
    !! Set the time-step to infinity and method to fully implicit
    dt_help = this%dt
    ab_help = this%ab
    cf_help = this%cf
    
    this%dt = huge(zero)
    this%ab = one
    this%cf = one
    
    !! Solve for conductive state at degree zero
    call this%prepare_mat_temp_sub()
    call this%solve_temp_ij_sub(0)
    
    !! Set the time-stepping to the initial choice
    this%dt = dt_help
    this%ab = ab_help
    this%cf = cf_help
    
    !! Add random perturbation to the conductive state
    !$omp parallel do private (is,ij,im,realp,imagp)
    do ir = 1, this%nd+1
      is  = 2*(ir-1)+1
      
      do ij = 1, this%jmax
        im = 0
          call random_number( realp ); realp = realp / 1e3
          
          this%temp(ij)%sol(im,is) = r2c_fn( realp )
        
        do im = 1, ij
          call random_number( realp ); realp = realp / 1e3
          call random_number( imagp ); imagp = imagp / 1e3
          
          this%temp(ij)%sol(im,is) = cmplx( realp, imagp, kind=dbl )
        end do
      end do
    end do
    !$omp end parallel do
    
  end subroutine init_conduction_sub
  
  subroutine init_fromFile_sub(this)
    class(T_ocean),  intent(inout) :: this
    integer                        :: ir, ij, ijm, ndI, jmsI, jmvI
    real(kind=dbl),    allocatable :: rrI(:)
    complex(kind=dbl), allocatable :: velcI(:,:), tempI(:,:), spher1I(:,:), torrI(:,:), spher2I(:,:), &
                                    & temp(:), spher1(:), spher2(:), torr(:)
    
    ndI  = nd_init_ocean+1
    jmsI = jm(jmax_init_ocean,jmax_init_ocean)
    jmvI = jml(jmax_init_ocean,jmax_init_ocean,+1)
    
    allocate( rrI(ndI), spher1I(jmsI,ndI), spher2I(jmsI,ndI), torrI(jmsI,ndI), tempI(jmsI,ndI), velcI(jmvI,ndI) )
    
    call read_3d_binfile_sub( 8, 'code/ocean/init_files/'//init_temp_file, ndI*jmsI, tempI, ndI, rrI )
    call read_3d_binfile_sub( 8, 'code/ocean/init_files/'//init_velc_file, ndI*jmvI, velcI, ndI, rrI )
    
    !$omp parallel do private (ijm)
    do ir = 1, ndI
      ijm = 1
        spher1I(ijm,ir) = czero
        torrI(  ijm,ir) = czero
        spher2I(ijm,ir) = czero
      
      do ijm = 2, jmsI
        spher1I(ijm,ir) = velcI(3*(ijm-1)-1,ir)
        torrI(  ijm,ir) = velcI(3*(ijm-1)+0,ir)
        spher2I(ijm,ir) = velcI(3*(ijm-1)+1,ir)
      end do
    end do
    !$omp end parallel do
    
    deallocate( velcI )
    
    !$omp parallel private (temp, spher1, spher2, torr)
    allocate( temp(this%jms), spher1(this%jms), spher2(this%jms), torr(this%jms) )
    
    !$omp do private (ij)
    do ir = 1, this%nd+1
      call this%rad_grid%interpolation_sub( this%jms, ir, temp,   ndI, jmsI, rrI, tempI   )
      call this%rad_grid%interpolation_sub( this%jms, ir, spher1, ndI, jmsI, rrI, spher1I )
      call this%rad_grid%interpolation_sub( this%jms, ir, spher2, ndI, jmsI, rrI, spher2I )
      call this%rad_grid%interpolation_sub( this%jms, ir, torr,   ndI, jmsI, rrI, torrI   )
      
      do ij = 0, this%jmax
        call copy_carray_sub( ij+1, temp(jm(ij,0)),   this%temp(ij)%sol(0,2*(ir-1)+1) )
        call copy_carray_sub( ij+1, torr(jm(ij,0)),   this%torr(ij)%sol(0,2*(ir-1)+1) )
        call copy_carray_sub( ij+1, spher1(jm(ij,0)), this%mech(ij)%sol(0,5*(ir-1)+1) )
        call copy_carray_sub( ij+1, spher2(jm(ij,0)), this%mech(ij)%sol(0,5*(ir-1)+2) )
      end do
    end do
    
    deallocate( temp, spher1, spher2, torr )
    !$omp end parallel
    
    deallocate( rrI, spher1I, spher2I, torrI, tempI )
    
  end subroutine init_fromFile_sub
  
  module procedure init_state_ocean_sub
    real(kind=dbl) :: ab_help, cf_help
    
    if ( init_through_file_ocean ) then
      call init_fromFile_sub(this)
    else
      call init_conduction_sub(this)
    end if
    
    !! First step with implicit stepping
    ab_help = this%ab
    cf_help = this%cf
    
    this%ab = one
    this%cf = one
    
    call this%prepare_mat_temp_sub()
    call this%prepare_mat_torr_sub()
    call this%prepare_mat_mech_sub()
    
    call this%time_scheme_sub()
    
    !! Set the time-step to the initial choice
    this%ab = ab_help
    this%cf = cf_help
    
    call this%prepare_mat_temp_sub()
    call this%prepare_mat_torr_sub()
    call this%prepare_mat_mech_sub()
    
  end procedure init_state_ocean_sub
  
end submodule init_state