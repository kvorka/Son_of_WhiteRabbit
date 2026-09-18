submodule (ocean) init
  implicit none; contains
  
  module procedure init_ocean_sub
    integer                        :: ir, is, ij, im, ijm, ik, error, ndI, jmsI, jmvI
    real(kind=dbl)                 :: ab_help, cf_help, dt_help, normFlux
    real(kind=dbl),    allocatable :: rrI(:)
    complex(kind=dbl)              :: flux
    complex(kind=dbl), allocatable :: velcI(:,:), tempI(:,:), spher1I(:,:), torrI(:,:), spher2I(:,:), &
                                    & temp(:), spher1(:), spher2(:), torr(:)
    
    !! Initialize the code baseline
    call this%init_objects_sub( nd = nd_ocean, jmax = jmax_ocean, r_ud = r_ud_ocean )
    
    !! Set the numerical and physical parameters
    this%cf = 0.6_dbl
    this%ab = 1.5_dbl
    
    this%Pr = Pr_ocean
    this%Ra = Ra_ocean
    this%Ek = Ek_ocean
    
    this%diffusion_type = diffusion_ocean
    this%thermal_bnd    = therm_bnd_ocean
    this%mechanical_bnd = mech_bnd_ocean
    
    !! Prepare Nusselt and Energy saving check files if needed
    if ( .not. speed ) then
      open(unit=11, file='data/Nuss.dat', status='new', action='write')
      open(unit=12, file='data/Laws.dat', status='new', action='write')
    end if
    
    !! Initialize the equations: right-hand sides, matrices, arrays for solutions
    allocate( this%temp(0:this%jmax) )
    allocate( this%torr(0:this%jmax) )
    allocate( this%mech(0:this%jmax) )
    
    !$omp parallel do private (ij)
    do ik = 0, (this%jmax-1)/2
      ij = ik
        call this%temp(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%torr(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%mech(ij)%init_sub( mm=ij, nvar=5*this%nd+2, nrhs=this%nd+1, ld=9, lu=8, def_rhs2=.true.  )
      
      ij = this%jmax-ik
        call this%temp(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%torr(ij)%init_sub( mm=ij, nvar=2*this%nd+1, nrhs=this%nd+1, ld=3, lu=3, def_rhs2=.false. )
        call this%mech(ij)%init_sub( mm=ij, nvar=5*this%nd+2, nrhs=this%nd+1, ld=9, lu=8, def_rhs2=.true.  )
    end do
    !$omp end parallel do
    
    !! Initialize the non-linear terms
    allocate( this%ntemp(this%jms,2:this%nd) )
    allocate( this%ntorr(this%jms,2:this%nd) )
    allocate( this%nsph1(this%jms,2:this%nd) )
    allocate( this%nsph2(this%jms,2:this%nd) )
    
    !$omp parallel do
    do ir = 2, this%nd
      call zero_carray_sub( this%jms, this%ntemp(1,ir) )
      call zero_carray_sub( this%jms, this%ntorr(1,ir) )
      call zero_carray_sub( this%jms, this%nsph1(1,ir) )
      call zero_carray_sub( this%jms, this%nsph2(1,ir) )
    end do
    !$omp end parallel do
    
    !! Set the thermal bottom boundary condition and compute
    !! the initial state of the ocean
    if ( init_through_file_bnd_ocean ) then
      
      select case ( this%thermal_bnd )
        case ('fluxd')
          
          open(unit=35, file='code/ocean/init_files/'//init_bbnd_file, status='old', action='read')
            !! mean value, i.e. degree 0
            read(35,*,iostat=error) ij, im, flux
            
            if ( ij /= 0 ) then
              write(*,*) 'invalid initflux file'
              stop
            else
              this%temp(0)%rhs1(0,1)%re = s4pi
              this%temp(0)%rhs1(0,1)%im = zero
              
              normFlux = flux%re / s4pi
            end if
            
            !! rest of degrees and orders
            do
              read(35,*,iostat=error) ij, im, flux
              
              if ( ( error == 0 ) .and. ( ij <= this%jmax ) ) then
                this%temp(ij)%rhs1(im,1) = flux / normFlux
              else
                exit
              end if
            end do
          close(35)
      end select
      
    else
      this%temp(0)%rhs1(0,1)%re = s4pi
      this%temp(0)%rhs1(0,1)%im = zero
      
    end if
    
    !! Initialize the variables either with saved arrays or
    !! with conductive profile and small random perturbation
    if ( init_through_file_ocean ) then
      
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
      
    else
      
      !! Set the time-step to infinity and method to fully implicit
      !! for computation of the conductive profile
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
      !$omp parallel do private (is,ij,im)
      do ir = 1, this%nd+1
        is  = 2*(ir-1)+1
        
        do ij = 1, this%jmax
          im = 0
            call random_number( this%temp(ij)%sol(im,is)%re )
            
            this%temp(ij)%sol(im,is)%re = this%temp(ij)%sol(im,is)%re / 1e3
            this%temp(ij)%sol(im,is)%im = zero
            
          do im = 1, ij
            call random_number( this%temp(ij)%sol(im,is)%re )
            call random_number( this%temp(ij)%sol(im,is)%im )
            
            this%temp(ij)%sol(im,is)%re = this%temp(ij)%sol(im,is)%re / 1e3
            this%temp(ij)%sol(im,is)%im = this%temp(ij)%sol(im,is)%im / 1e3
          end do
        end do
      end do
      !$omp end parallel do
      
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
    
    !! Set the time-step to the initial choice for the rest of the computation
    this%ab = ab_help
    this%cf = cf_help
    
    call this%prepare_mat_temp_sub()
    call this%prepare_mat_torr_sub()
    call this%prepare_mat_mech_sub()
    
  end procedure init_ocean_sub
  
  module procedure deallocate_ocean_sub
    
    close(11)
    close(12)
    
    deallocate( this%ntemp )
    deallocate( this%ntorr )
    deallocate( this%nsph1 )
    deallocate( this%nsph2 )
    
    call this%deallocEqs_sub( this%temp )
    call this%deallocEqs_sub( this%torr )
    call this%deallocEqs_sub( this%mech )
    
    call this%deallocate_objects_sub()
    
  end procedure deallocate_ocean_sub
  
end submodule init