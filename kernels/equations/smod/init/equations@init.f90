submodule (equations) init
  implicit none; contains
  
  module procedure init_equations_sub
    
    !! Number of variables
    this%n   = nvar
    this%mm  = mm
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
    
    !! Memory allocation for solution and right-hand side
    allocate( this%sol(0:mm,this%n), this%rhs1(0:mm,nrhs), this%nlm1(0:mm,2:nrhs-1) )
    
    !! Optional right-hand side
    if ( def_rhs2 ) allocate( this%rhs2(0:mm,1:nrhs  ) )
    if ( def_nlm2 ) allocate( this%nlm2(0:mm,2:nrhs-1) )
    
  end procedure init_equations_sub
  
  module procedure deallocate_equations_sub
    
    deallocate( this%M )
    deallocate( this%U )
    deallocate( this%L )
    deallocate( this%I )
    
    deallocate( this%sol  )
    deallocate( this%rhs1 )
    deallocate( this%nlm1 )
    
    if ( allocated( this%rhs2 ) ) deallocate( this%rhs2 )
    if ( allocated( this%nlm2 ) ) deallocate( this%nlm2 )
    
  end procedure deallocate_equations_sub
  
end submodule init