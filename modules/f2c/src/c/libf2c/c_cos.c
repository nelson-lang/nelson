#include "nelson_f2c.h"

#ifdef KR_headers
extern double sin(), cos(), sinh(), cosh();

VOID c_cos(r, z) complex *r, *z;
#else
#undef abs
#include "math.h"
#undef complex
#include "nelson_f2c.h"
#ifdef __cplusplus
extern "C" {
#endif

void c_cos(complex *r, complex *z)
#endif
{
double zi = z->i, zr = z->r;
r->r = (real) (cos(zr) * cosh(zi));
r->i = (real) (- sin(zr) * sinh(zi));
}
#ifdef __cplusplus
}
#endif
