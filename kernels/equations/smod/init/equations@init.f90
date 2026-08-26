submodule (equations) init
  implicit none; contains
  
  module procedure init_equations_sub
    
    !! Number of variables
    this%n   = nvar
    this%mm1 = mm+1
    
    !! Variables needed for LU decomposition and multiplying 
    !! by a matrix during the computation
    this%ld  = ld
    this%lu  = lu
    this%ldu = ld+1+lu
    
    allocate( this%M( this%ldu, this%n ) ) ; this%M = zero
    allocate( this%U( this%ldu, this%n ) ) ; this%U = zero
    allocate( this%L( this%ld , this%n ) ) ; this%L = zero
    allocate( this%I( this%n )           ) ; this%I = 0
    
    !! Memory allocation for solution, right hand side 
    !! and optional second rhs holder
    allocate( this%sol(0:mm,this%n) )
    allocate( this%rhs1(0:mm,nrhs)  )
    
    if ( def_rhs2 ) then
      allocate( this%rhs2(0:mm,nrhs)  )
    end if
    
  end procedure init_equations_sub
  
  module procedure deallocate_equations_sub
    
    deallocate( this%M )
    deallocate( this%U )
    deallocate( this%L )
    deallocate( this%I )
    
    deallocate( this%sol  )
    deallocate( this%rhs1 )
    
    if ( allocated(this%rhs2) ) then
      deallocate( this%rhs2 )
    end if
    
  end procedure deallocate_equations_sub
  
end submodule init