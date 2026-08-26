submodule (ocean) init
  implicit none; contains
  
  module procedure init_ocean_sub
    
    call this%init_objects_sub( nd = nd_ocean, jmax = jmax_ocean, r_ud = r_ud_ocean )
    
    this%n_iter = n_iter_ocean
    this%cf     = 0.6_dbl
    this%ab     = 1.5_dbl
    
    this%Pr = Pr_ocean
    this%Ra = Ra_ocean
    this%Ek = Ek_ocean
    
    this%diffusion_type = diffusion_ocean
    this%thermal_bnd    = therm_bnd_ocean
    
    if ( .not. present(speed) ) then
      open(unit=11, file='data/Nuss.dat', status='new', action='write')
      open(unit=12, file='data/Laws.dat', status='new', action='write')
    end if
    
    !! Initialize the equations: right-hand sides, matrices,
    !! arrays for solutions; together with non-linear terms
    call this%init_eq_all_sub()
    call this%init_nl_all_sub()
    
    !! Set the thermal bottom boundary condition and compute
    !! the initial state of the ocean
    call init_temp_bbnd_ocean_sub(this)
    call init_state_ocean_sub(this)
    
  end procedure init_ocean_sub
  
  module procedure deallocate_ocean_sub
    
    close(11)
    close(12)
    
    call this%deallocate_objects_sub()
    
  end procedure deallocate_ocean_sub
  
end submodule init