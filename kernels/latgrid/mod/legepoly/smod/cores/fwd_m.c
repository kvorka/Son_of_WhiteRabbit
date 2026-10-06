#include "../codelets/fwd_set.h"
#include "../codelets/fwd_rec.h"
#include "../codelets/fwd_rsc.h"

extern inline __attribute__((always_inline))
void fwd_m_c( const int n,
              const int m,
              const int nma,
              const double *restrict fmj,
              const double *restrict cosx,
              const double *restrict sinx,
              const double *restrict cosx2,
              const double *restrict weight,
              const double *restrict sumN,
              const double *restrict sumS, 
                    double *restrict pmm,
                    double *restrict pmj,
                    double *restrict pmj1,
                    double *restrict swork,
                    double *restrict cr )

{
    
    fwd_rsc_c( n, weight, cosx, sumN, sumS, swork );
    fwd_set_c( n, m, fmj, cosx, sinx, swork, pmm, pmj1, pmj, cr );
    fwd_rec_c( n, nma, fmj+3, cosx2, swork, pmj1, pmj, cr+4*n );
    
}

extern inline __attribute__((always_inline))
void fwd_end_c( const int n,
                const int m,
                const double *restrict fmj,
                const double *restrict cosx,
                const double *restrict sinx,
                const double *restrict weight,
                const double *restrict sumN,
                const double *restrict sumS, 
                      double *restrict pmm,
                      double *restrict pmj,
                      double *restrict pmj1,
                      double *restrict swork,
                      double *restrict cr )

{
    
    fwd_rsc_c( n, weight, cosx, sumN, sumS, swork );
    fwd_set_c( n, m, fmj, cosx, sinx, swork, pmm, pmj1, pmj, cr );
    
}