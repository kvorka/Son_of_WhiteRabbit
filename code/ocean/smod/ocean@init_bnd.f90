submodule (ocean) init_bnd
  implicit none; contains
  
  module procedure init_temp_bbnd_ocean_sub
    integer           :: ij, im, error
    real(kind=dbl)    :: normFlux
    complex(kind=dbl) :: flux
    
    if ( init_through_file_bnd_ocean ) then
      
      select case ( this%thermal_bnd )
        case ('fluxd')
          
          open(unit=35, file='code/ocean/init_files/'//init_bbnd_file, status='old', action='read')
            !! mean value
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
              
              if ( error == 0 ) then
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
    
  end procedure init_temp_bbnd_ocean_sub
  
end submodule init_bnd