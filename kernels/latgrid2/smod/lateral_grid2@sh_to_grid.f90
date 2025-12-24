submodule (lateral_grid2) sh_to_grid
  implicit none; contains
  
  module procedure jm_to_grid_shtns_sub
    
    call this%rxd%jm_to_shtns_sub( this%shtns_c, scal )
    call SH_to_spat( this%shtns_c, scal, grid )
    
  end procedure jm_to_grid_shtns_sub
  
  module procedure jml_to_grid_shtns_sub
    
    call this%rxd%jml_to_qst_sub( pol1, torr, pol2 )
    
    call this%rxd%jm_to_shtns_sub( this%shtns_c, pol1 )
    call this%rxd%jm_to_shtns_sub( this%shtns_c, torr )
    call this%rxd%jm_to_shtns_sub( this%shtns_c, pol2 )
    
    call SHqst_to_spat( this%shtns_c, pol1, pol2, torr, rgrid, tgrid, pgrid )
    
  end procedure jml_to_grid_shtns_sub
  
end submodule sh_to_grid