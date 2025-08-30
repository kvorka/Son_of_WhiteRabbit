submodule (physicalobject) output
  implicit none ; contains
  
  module procedure vypis_sub
    integer                        :: ir, ijm
    complex(kind=dbl), allocatable :: field(:)
    
    select case (quantity)
      case ('temperature')
        open(unit=filenum, file=path//'/Temp-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', status='new', action='write')
          allocate( field(this%jms) )
            do ir = 1, this%nd+1
              call this%temp_rr_ijm_sub(ir, field); write(filenum,*) this%rad_grid%rr(ir), field
            end do
          deallocate( field )
        close(filenum)
      
      case ('velocity')
        open(unit=filenum, file=path//'/Velc-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', status='new', action='write')
          allocate( field(this%jmv) )
            do ir = 1, this%nd+1
              call this%v_rr_ijml_sub(ir, field); write(filenum,*) this%rad_grid%rr(ir), field
            end do
          deallocate( field )
        close(filenum)
      
      case ('fluxu')
        open(unit=filenum, file=path//'/Fluxu-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', status='new', action='write')
          allocate( field(this%jms) )
            call this%dT_dr_r_ijm_sub(this%nd, field)
            
            do ijm = 1, this%jms
              write(filenum,*) ijm, -field(ijm)
            end do
          deallocate( field )
        close(filenum)
      
      case ('fluxd')
        open(unit=filenum, file=path//'/Fluxd-'//trim(adjustl(int2str_fn(this%poc)))//'.dat', status='new', action='write')
          allocate( field(this%jms) )
            call this%dT_dr_r_ijm_sub(1, field)
            
            do ijm = 1, this%jms
              write(filenum,*) ijm, -field(ijm)
            end do
          deallocate( field )
        close(filenum)
    end select
    
  end procedure vypis_sub
  
end submodule output