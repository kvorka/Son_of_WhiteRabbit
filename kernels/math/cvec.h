#ifndef CVEC_H
#define CVEC_H

/*********************************************************
    This part is going to be read by fortran prepro-
    cessor, therefore no indentation to make it pretty. 
**********************************************************/
#if defined ( mem32 )
#define vlen 4
#define alignement 32
#else
#define vlen 8
#define alignement 64
#endif

/*********************************************************
    This part is solely for C part of the code.
**********************************************************/
#if !defined( __GFORTRAN__ )
    #include <stddef.h>
    #include <immintrin.h>
        
    #if defined ( mem32 )
                
        typedef __m256d __td;
                
        #define _t_set1_pd(val)      _mm256_set1_pd(val)
        #define _t_load_pd(ptr)      _mm256_load_pd(ptr)
        #define _t_store_pd(ptr,val) _mm256_store_pd((ptr),(val))
        #define _t_add_pd(a, b)      _mm256_add_pd((a),(b))
        #define _t_sub_pd(a, b)      _mm256_sub_pd((a),(b))
        #define _t_mul_pd(a, b)      _mm256_mul_pd((a),(b))
                
        #if defined (__FMA__)
            #define _t_fmadd_pd(a, b, c)  _mm256_fmadd_pd((a),(b),(c))
            #define _t_fmsub_pd(a, b, c)  _mm256_fmsub_pd((a),(b),(c))
            #define _t_fnmadd_pd(a, b, c) _mm256_fnmadd_pd((a),(b),(c))
            #define _t_fnmsub_pd(a, b, c) _mm256_fnmsub_pd((a),(b),(c))
        #endif
                
    #else
                
        typedef __m512d __td;
                
        #define _t_set1_pd(val)       _mm512_set1_pd(val)
        #define _t_load_pd(ptr)       _mm512_load_pd(ptr)
        #define _t_store_pd(ptr,val)  _mm512_store_pd((ptr),(val))
        #define _t_add_pd(a, b)       _mm512_add_pd((a),(b))
        #define _t_sub_pd(a, b)       _mm512_sub_pd((a),(b))
        #define _t_mul_pd(a, b)       _mm512_mul_pd((a),(b))
        #define _t_fmadd_pd(a, b, c)  _mm512_fmadd_pd((a),(b),(c))
        #define _t_fmsub_pd(a, b, c)  _mm512_fmsub_pd((a),(b),(c))
        #define _t_fnmadd_pd(a, b, c) _mm512_fnmadd_pd((a),(b),(c))
        #define _t_fnmsub_pd(a, b, c) _mm512_fnmsub_pd((a),(b),(c))
                
    #endif
#endif

#endif