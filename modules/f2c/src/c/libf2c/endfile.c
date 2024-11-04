#include "nelson_f2c.h"
#include "fio.h"

/* Compile this with -DNO_TRUNCATE if unistd.h does not exist or */
/* if it does not define int truncate(const char *name, off_t). */

#ifdef MSDOS
#undef NO_TRUNCATE
#define NO_TRUNCATE
#endif

#ifndef NO_TRUNCATE
#include "unistd.h"
#endif

#ifdef KR_headers
extern char*
strcpy();
extern FILE*
tmpfile();
#else
#undef abs
#undef min
#undef max
#include "stdlib.h"
#include "string.h"
#ifdef __cplusplus
extern "C"
{
#endif
#endif

extern char *f__r_mode[], *f__w_mode[];

#ifdef KR_headers
integer
f_end(a)
alist* a;
#else
    integer
    f_end(alist* a)
#endif
{
    unit* b;
    FILE* tf;
    if (a->aunit >= MXUNIT || a->aunit < 0) {
        err(a->aerr, 101, "endfile");
    }
    b = &f__units[a->aunit];
    if (b->ufd == NULL) {
        char nbuf[10];
        snprintf(nbuf, sizeof(nbuf), "fort.%ld", (long)a->aunit);
        if (tf = FOPEN(nbuf, f__w_mode[0])) {
            fclose(tf);
        }
        return (0);
    }
    b->uend = 1;
    return (b->useek ? t_runc(a) : 0);
}

#ifdef NO_TRUNCATE
static int
#ifdef KR_headers
copy(from, len, to)
FILE *from, *to;
register long len;
#else
copy(FILE* from, register long len, FILE* to)
#endif
{
    int len1;
    char buf[BUFSIZ];
    while (fread(buf, len1 = len > BUFSIZ ? BUFSIZ : (int)len, 1, from)) {
        if (!fwrite(buf, len1, 1, to)) {
            return 1;
        }
        if ((len -= len1) <= 0) {
            break;
        }
    }
    return 0;
}
#endif /* NO_TRUNCATE */

int
#ifdef KR_headers
t_runc(a)
alist* a;
#else
    t_runc(alist* a)
#endif
{
    OFF_T loc, len;
    unit* b;
    int rc;
    FILE* bf;
#ifdef NO_TRUNCATE
    FILE* tf;
#endif
    b = &f__units[a->aunit];
    if (b->url) {
        return (0); /*don't truncate direct files*/
    }
    loc = FTELL(bf = b->ufd);
    FSEEK(bf, (OFF_T)0, SEEK_END);
    len = FTELL(bf);
    if (loc >= len || b->useek == 0) {
        return (0);
    }
    if (rc) {
        err(a->aerr, 111, "endfile");
    }
    return 0;
}
#ifdef __cplusplus
}
#endif
