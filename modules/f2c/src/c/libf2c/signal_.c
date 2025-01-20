#ifdef _MSC_VER
#pragma warning(disable : 4311)
#endif
#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#endif

#include "nelson_f2c.h"
#include "signal1.h"
#ifdef __cplusplus
extern "C" {
#endif

ftnint
#ifdef KR_headers
signal_(sigp, proc) integer *sigp;
sig_pf proc;
#else
signal_(integer *sigp, sig_pf proc)
#endif
{
    int sig;
    sig = (int)*sigp;
    return (ftnint)signal(sig, proc);
}
#ifdef __cplusplus
}
#endif
