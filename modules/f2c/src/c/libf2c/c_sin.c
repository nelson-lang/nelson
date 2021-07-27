#include "nelson_f2c.h"

#ifdef KR_headers
extern double sin(), cos(), sinh(), cosh();

VOID c_sin(r, z) complex *r, *z;
#else
#undef abs
#include "math.h"
#undef complex
#include "nelson_f2c.h"
#ifdef __cplusplus
extern "C" {
#endif

void c_sin(complex *r, complex *z)
#endif
{
double zi = z->i, zr = z->r;
r->r = (real)(sin(zr) * cosh(zi));
r->i = (real)(cos(zr) * sinh(zi));
}
#ifdef __cplusplus
}
#endif
