submodule (physicalobject) output
  implicit none ; contains
  
  module procedure writefile_sub
    integer                        :: ir
    complex(kind=dbl), allocatable :: arr(:,:)
    
    select case (quantity)
      case ('temperature')
        allocate( arr(this%jms,this%nd+1) )
        
        !$omp parallel do
        do ir = 1, this%nd+1
          call this%temp_rr_jm_sub( ir, arr(1,ir) )
        end do
        !$omp end parallel do
        
        call write_3d_binfile_sub( filenum, path//'/Temp-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', &
                                 & (this%nd+1)*this%jms, arr, this%nd+1, this%rad_grid%rr )
      
      case ('velocity')
        allocate( arr(this%jmv,this%nd+1) )
        
        !$omp parallel do
        do ir = 1, this%nd+1
          call this%velc_rr_jml_sub( ir, arr(1,ir) )
        end do
        !$omp end parallel do
        
        call write_3d_binfile_sub( filenum, path//'/Velc-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', &
                                 & (this%nd+1)*this%jmv, arr, this%nd+1, this%rad_grid%rr )
        
      case ('fluxu')
        allocate( arr(this%jms,1) )
        
        call this%dT_dr_r_jm_sub( this%nd, arr(1,1) )
        
        call write_2d_binfile_sub( filenum, path//'/Fluxu-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', this%jms, arr )
      
      case ('fluxd')
        allocate( arr(this%jms,1) )
        
        call this%dT_dr_r_jm_sub( 1, arr(1,1) )
        
        call write_2d_binfile_sub( filenum, path//'/Fluxd-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', this%jms, arr )
    end select
    
    deallocate( arr )
    
  end procedure writefile_sub
  
end submodule output
