module spharray
  use math
  implicit none
  
  type, public :: T_spharray
    complex(kind=dbl), allocatable :: arr(:,:)
    
    contains
    
    procedure :: init_sub       => init_spharray_sub
    procedure :: deallocate_sub => deallocate_spharray_sub
    
  end type T_spharray
  
  interface
    module subroutine init_spharray_sub(this, mm, nd)
      class(T_spharray), intent(inout) :: this
      integer,           intent(in)    :: nd, mm
    end subroutine init_spharray_sub
    
    module subroutine deallocate_spharray_sub(this)
      class(T_spharray), intent(inout) :: this
    end subroutine deallocate_spharray_sub
  end interface

end module spharray