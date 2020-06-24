#ifndef _a_h_
#define _a_h_

#define ASSERT

#ifdef ASSERT

#define EA(expr) \
  ((!(expr))?(printf( "Assert %s %d: %s\n", __FILE__, __LINE__, #expr ),exit(1),0):0)
#define A(expr) if (!(expr)) \
  (printf( "Assert %s %d: %s\n", __FILE__, __LINE__, #expr ),exit(1))
#define AA(expr,rept) if (!(expr)) do { printf rept; exit( 1 ); } while(0)
#define AB(code) do code while (0)

#else

#define A(expr)
#define AA(expr,rept)
#define AB(code) do {} while (0)

#endif

#endif /* _a_h_ */
