submodule (ocean) init_bnd
  implicit none; contains
  
  module procedure init_temp_bbnd_ocean_sub
    integer           :: ij, im, error
    complex(kind=dbl) :: flux
    
    !if ( init_through_file_bnd_ocean ) then
    !  select case ( this%thermal_bnd )
    !    case ('fluxd')
    !      
    !      open(unit=35, file='code/ocean/heat_flux.cmplx', status='old', action='read')
    !        do
    !          read(35,*,iostat=error) ij, im, flux
    !          
    !          if ( error == 0 ) then
    !            this%rtemp(1,jm(ij,im)) = flux
    !          else
    !            exit
    !          end if
    !        end do
    !      close(35)
    !      
    !      this%rtemp(1,:) = this%rtemp(1,:) / ( this%rtemp(1,1)%re / s4pi )
    !  end select
    !else
    this%temp(0)%rhs1(0,1) = cs4pi
    !end if
    
  end procedure init_temp_bbnd_ocean_sub
  
end submodule init_bnd