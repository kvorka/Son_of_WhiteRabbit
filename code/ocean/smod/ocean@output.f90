submodule (ocean) output
  implicit none; contains
  
  module procedure vypis_ocean_sub
  
#if defined ( benchmark )
    write(11,*) this%poc, this%t, this%nuss_fn(), this%reynolds_fn(choice='convective')
#else
    write(11,*) this%poc, this%t, this%nuss_fn(), this%reynolds_fn()
#endif
    
    write(12,*) this%poc, this%t, c2r_fn( this%dT_dr_r_fn(this%nd,0,0) ) / &
                                & c2r_fn( this%dT_dr_r_fn(1,0,0)       ) / this%r_ud**2
    
    call this%vypis_sub(8, 'data/data_ocean_temp' , 'temperature')
    call this%vypis_sub(8, 'data/data_ocean_veloc', 'velocity'   )
    call this%vypis_sub(8, 'data/data_ocean_fluxu', 'fluxu'      )
    
    this%poc = this%poc + 1
    
  end procedure vypis_ocean_sub
  
end submodule output