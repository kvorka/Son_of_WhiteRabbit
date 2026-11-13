submodule (ocean) output
  implicit none; contains
  
  module procedure write_state_ocean_sub
    
    write(11,*) this%poc, this%t / ( 2 * pi ), this%bnd_flux_fn(), this%nuss_fn(), this%reynolds_c_fn() / this%vEk, &
                                             & this%reynolds_z_fn() / this%vEk, this%reynolds_fn() / this%vEk
    
    call this%writefile_sub( 8, 'data/data_ocean_temp' , 'temperature' )
    call this%writefile_sub( 8, 'data/data_ocean_veloc', 'velocity'    )
    call this%writefile_sub( 8, 'data/data_ocean_fluxu', 'fluxu'       )
    
    this%poc = this%poc + 1
    
  end procedure write_state_ocean_sub
  
end submodule output