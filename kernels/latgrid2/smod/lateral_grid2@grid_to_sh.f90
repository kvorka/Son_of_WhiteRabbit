submodule (lateral_grid2) grid_to_sh
  implicit none; contains
  
  module procedure grid_to_jm_shtns_sub
    
    call spat_to_SH( this%shtns_c, grid, scal )
    call this%rxd%shtns_to_jm_sub( this%shtns_c, scal )
    
  end procedure grid_to_jm_shtns_sub
  
  module procedure grid_to_jml_shtns_sub
    
    call spat_to_SHqst( this%shtns_c, rgrid, tgrid, pgrid, pol1, pol2, torr )
    
    call this%rxd%shtns_to_jm_sub( this%shtns_c, pol1 )
    call this%rxd%shtns_to_jm_sub( this%shtns_c, torr )
    call this%rxd%shtns_to_jm_sub( this%shtns_c, pol2 )
    
    call this%rxd%qst_to_jml_sub( pol1, torr, pol2 )
    
  end procedure grid_to_jml_shtns_sub
  
end submodule grid_to_sh