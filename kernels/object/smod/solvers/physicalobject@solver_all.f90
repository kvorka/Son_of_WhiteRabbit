submodule (physicalobject) solver_all
  implicit none ; contains
  
  module procedure solve_all_sub
    integer :: ik
    
    !$omp parallel do
    do ik = 0, (this%jmax-1)/2
      call this%solve_temp_ij_sub(ik)
      call this%solve_temp_ij_sub(this%jmax-ik)
      
      call this%solve_torr_ij_sub(ik)
      call this%solve_torr_ij_sub(this%jmax-ik)
      
      call this%solve_mech_ij_sub(ik)
      call this%solve_mech_ij_sub(this%jmax-ik)
    end do
    !$omp end parallel do
      
  end procedure solve_all_sub
  
end submodule solver_all