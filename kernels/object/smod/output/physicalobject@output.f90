submodule (physicalobject) output
  implicit none ; contains
  
  module procedure write_binfile_sub
    
    open( unit=filenum, file=filepath, form='unformatted', access='stream', status='new', action='write')
    
    select case (status)
      case ('3dtemp')
        write(filenum) this%rad_grid%rr(1:this%nd+1), arr(1:this%jms*(this%nd+1))
      
      case('3dvelc')
        write(filenum) this%rad_grid%rr(1:this%nd+1), arr(1:this%jmv*(this%nd+1))
      
      case ('2dflux')
        write(filenum) arr(1:this%jms)
    end select
    
    close(filenum)
    
  end procedure write_binfile_sub
  
  module procedure vypis_sub
    integer                        :: ir
    complex(kind=dbl), allocatable :: arr(:,:)
    
    select case (quantity)
      case ('temperature')
        allocate( arr(this%jms,this%nd+1) )
        
        !$omp parallel do
        do ir = 1, this%nd+1
          call this%temp_rr_jm_sub(ir, arr(:,ir))
        end do
        !$omp end parallel do
        
        call this%write_binfile_sub( filenum, path//'/Temp-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', arr, '3dtemp' )
      
      case ('velocity')
        allocate( arr(this%jmv,this%nd+1) )
        
        !$omp parallel do
        do ir = 1, this%nd+1
          call this%velc_rr_jml_sub(ir, arr(:,ir))
        end do
        !$omp end parallel do
        
        call this%write_binfile_sub( filenum, path//'/Velc-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', arr, '3dvelc' )
        
      case ('fluxu')
        allocate( arr(this%jms,1) )
        
        call this%dT_dr_r_jm_sub( this%nd, arr(:,1) )
        
        call this%write_binfile_sub( filenum, path//'/Fluxu-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', arr, '2dflux' )
      
      case ('fluxd')
        allocate( arr(this%jms,1) )
        
        call this%dT_dr_r_jm_sub( 1, arr(:,1) )
        
        call this%write_binfile_sub( filenum, path//'/Fluxd-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', arr, '2dflux' )
    end select
    
    deallocate( arr )
    
  end procedure vypis_sub
  
end submodule output
