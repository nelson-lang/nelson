//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "DotPower.hpp"
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
    /**
    * These are the power functions...
    */
    // cicPower --> pow_zi
    // cfcPower --> pow_zz
    // zdzPower --> pow_zz
    // cccPower --> pow_zz
    // zzzPower --> pow_zz
    // zizPower --> pow_zi
    // didPower --> pow_di
    // dddPower --> pow_dd
    // fifPower --> pow_ri
    // fffPower --> pow_dd


    template <class T>
    T complex_abs(T real, T imag)
    {
        double temp;
        if (real < 0)
        {
            real = -real;
        }
        if (imag < 0)
        {
            imag = -imag;
        }
        if (imag > real)
        {
            temp = real;
            real = imag;
            imag = (T)(temp);
        }
        if ((real + imag) == real)
        {
            return(real);
        }
        temp = imag / real;
        temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
        return (T)(temp);
    }
    //=============================================================================
    template <class T>
    void complex_divide(T* c, const T* a, const T* b)
    {
        double ratio, den;
        double abr, abi, cr;
        if ((abr = b[0]) < 0.)
        {
            abr = -abr;
        }
        if ((abi = b[1]) < 0.)
        {
            abi = -abi;
        }
        if (abr <= abi)
        {
            if (abi == 0)
            {
                if (a[1] != 0 || a[0] != 0)
                {
                    abi = 1.;
                }
                c[1] = c[0] = (float)(abi / abr);
                return;
            }
            ratio = b[0] / b[1];
            den = b[1] * (1 + ratio*ratio);
            cr = (a[0] * ratio + a[1]) / den;
            c[1] = (T)((a[1] * ratio - a[0]) / den);
        }
        else
        {
            ratio = b[1] / b[0];
            den = b[0] * (1 + ratio*ratio);
            cr = (a[0] + a[1] * ratio) / den;
            c[1] = (T)((a[1] - a[0] * ratio) / den);
        }
        c[0] = (T)(cr);
    }
    //=============================================================================
    void power_zi(double *p, double *a, int b)  	/* p = a**b  */
    {
        int n;
        unsigned long u;
        double t;
        double q[2], x[2];
        static double one[2] = { 1.0, 0.0 };
        n = b;
        q[0] = 1;
        q[1] = 0;
        if (n == 0)
        {
            goto done;
        }
        if (n < 0)
        {
            n = -n;
            complex_divide<double>(x, one, a);
        }
        else
        {
            x[0] = a[0];
            x[1] = a[1];
        }
        for (u = n;;)
        {
            if (u & 01)
            {
                t = q[0] * x[0] - q[1] * x[1];
                q[1] = q[0] * x[1] + q[1] * x[0];
                q[0] = t;
            }
            if (u >>= 1)
            {
                t = x[0] * x[0] - x[1] * x[1];
                x[1] = 2 * x[0] * x[1];
                x[0] = t;
            }
            else
            {
                break;
            }
        }
done:
        p[1] = q[1];
        p[0] = q[0];
    }
    //=============================================================================
    void power_zz(double *c, double *a, double *b)
    {
        double logr, logi, x, y;
        logr = log(complex_abs<double>(a[0], a[1]));
        logi = atan2(a[1], a[0]);
        x = exp(logr * b[0] - logi * b[1]);
        y = logr * b[1] + logi * b[0];
        c[0] = x * cos(y);
        c[1] = x * sin(y);
    }
    //=============================================================================
    double power_di(double a, int b)
    {
        double pow, x;
        int n;
        pow = 1;
        x = a;
        n = b;
        if (n != 0)
        {
            if (n < 0)
            {
                n = -n;
                x = 1 / x;
            }
            for (unsigned long u = n;;)
            {
                if (u & 01)
                {
                    pow *= x;
                }
                if (u >>= 1)
                {
                    x *= x;
                }
                else
                {
                    break;
                }
            }
        }
        return(pow);
    }
    //=============================================================================
    double power_dd(double a, double b)
    {
        return pow(a, b);
    }
    //=============================================================================
    void cicpower(int n, float *c, float *a, int stride1, int *b,
                  int stride2)
    {
        int m, p;
        double z1[2];
        double z3[2];
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            z1[0] = a[2 * m];
            z1[1] = a[2 * m + 1];
            power_zi(z3, z1, b[p]);
            c[2 * i] = (float)z3[0];
            c[2 * i + 1] = (float)z3[1];
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void cfcpower(int n, float *c, float *a, int stride1, float *b,
                  int stride2)
    {
        int m, p;
        double z1[2];
        double z2[2];
        double z3[2];
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            z1[0] = a[2 * m];
            z1[1] = a[2 * m + 1];
            z2[0] = b[p];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = (float)z3[0];
            c[2 * i + 1] = (float)z3[1];
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void zdzpower(int n, double *c, double *a, int stride1,
                  double *b, int stride2)
    {
        int m, p;
        double z1[2];
        double z2[2];
        double z3[2];
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            z1[0] = a[2 * m];
            z1[1] = a[2 * m + 1];
            z2[0] = b[p];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void cccpower(int n, float *c, float *a, int stride1, float *b,
                  int stride2)
    {
        int m, p;
        double z1[2];
        double z2[2];
        double z3[2];
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            z1[0] = a[2 * m];
            z1[1] = a[2 * m + 1];
            z2[0] = b[2 * p];
            z2[1] = b[2 * p + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = (float)z3[0];
            c[2 * i + 1] = (float)z3[1];
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void zzzpower(int n, double *c, double *a, int stride1, double *b,
                  int stride2)
    {
        int m, p;
        double z1[2];
        double z2[2];
        double z3[2];
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            z1[0] = a[2 * m];
            z1[1] = a[2 * m + 1];
            z2[0] = b[2 * p];
            z2[1] = b[2 * p + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void zizpower(int n, double *c, double *a, int stride1, int *b,
                  int stride2)
    {
        int m, p;
        double z1[2];
        double z3[2];
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            z1[0] = a[2 * m];
            z1[1] = a[2 * m + 1];
            power_zi(z3, z1, b[p]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void didpower(int n, double *c, double *a, int stride1, int *b,
                  int stride2)
    {
        int m, p;
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            c[i] = power_di(a[m], b[p]);
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void dddpower(int n, double *c, double *a, int stride1, double *b,
                  int stride2)
    {
        int m, p;
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            c[i] = power_dd(a[m], b[p]);
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void fifpower(int n, float *c, float *a, int stride1, int *b,
                  int stride2)
    {
        int m, p;
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            c[i] = (float)power_di(a[m], b[p]);
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    void fffpower(int n, float *c, float *a, int stride1, float *b,
                  int stride2)
    {
        int m, p;
        m = 0;
        p = 0;
        for (int i = 0; i<n; i++)
        {
            c[i] = (float)power_dd(a[m], b[p]);
            m += stride1;
            p += stride2;
        }
    }
    //=============================================================================
    /**
    * This is the generic function interface into calculations
    * that can be performed on some type.
    */
    typedef void(*vvfun) (indexType length,
                          void* result,
                          const void* arg1,
                          const int stride1,
                          const void* arg2,
                          const int stride2);
    //=============================================================================
    /**
    * This structure holds pointers to functions when
    * all types can be handled.
    */
    /*
    typedef struct {
    	vvfun int32func;
    	vvfun floatfunc;
    	vvfun doublefunc;
    	vvfun complexfunc;
    	vvfun dcomplexfunc;
    } packVectorVector;
    */
    //=============================================================================
    inline ArrayOf doPowerAssist(ArrayOf A, Class AClass,
                                 ArrayOf B, Class BClass,
                                 Class CClass, vvfun exec)
    {
        ArrayOf C;
        A.promoteType(AClass);
        B.promoteType(BClass);
        if (A.isScalar())
        {
            indexType Blen(B.getLength());
            C = ArrayOf(CClass, B.getDimensions(), NULL);
            void * Cp = C.allocateArrayOf(CClass, Blen);
            exec(Blen, Cp, A.getDataPointer(), 0, B.getDataPointer(), 1);
            C.setDataPointer(Cp);
        }
        else if (B.isScalar())
        {
            indexType Alen(A.getLength());
            C = ArrayOf(CClass, A.getDimensions(), NULL);
            void* Cp = C.allocateArrayOf(CClass, Alen);
            exec(Alen, Cp, A.getDataPointer(), 1, B.getDataPointer(), 0);
            C.setDataPointer(Cp);
        }
        else
        {
            indexType Alen(A.getLength());
            C = ArrayOf(CClass, A.getDimensions(), NULL);
            void* Cp = C.allocateArrayOf(CClass, Alen);
            exec(Alen, Cp, A.getDataPointer(), 1, B.getDataPointer(), 1);
            C.setDataPointer(Cp);
        }
        return C;
    }
    //=============================================================================
#define OPCASE(t,o) case t: opType = o; break;
#define MAPOP(o,a,b,c,f) case o: return doPowerAssist(A,a,B,b,c,f);
    inline ArrayOf DoPowerTwoArgFunction(ArrayOf A, ArrayOf B) throw(Exception)
    {
        ArrayOf C;
        bool Anegative;
        stringVector dummySV;
        Class AClass, BClass;
        int opType;
        if (A.isEmpty() || B.isEmpty())
        {
            return ArrayOf::emptyConstructor();
        }
        CheckNumeric(A, B, "^");
        if (!(SameSizeCheck(A.getDimensions(), B.getDimensions()) || A.isScalar() || B.isScalar()))
        {
            throw Exception(L"Size mismatch on arguments to power (^) operator.");
        }
        // If A is not at least a float type, promote it to double
        AClass = A.getDataClass();
        BClass = B.getDataClass();
        if (AClass < NLS_SINGLE)
        {
            AClass = NLS_DOUBLE;
        }
        if (BClass < NLS_INT64)
        {
            BClass = NLS_INT64;
        }
        // Get a read on if A is positive
        Anegative = !(A.isPositive());
        // Check through the different type cases...
        opType = 0;
        if (AClass == NLS_SCOMPLEX)
        {
            switch (BClass)
            {
                    OPCASE(NLS_INT32, 1);
                    OPCASE(NLS_SINGLE, 2);
                    OPCASE(NLS_DOUBLE, 3);
                    OPCASE(NLS_SCOMPLEX, 4);
                    OPCASE(NLS_DCOMPLEX, 5);
            }
        }
        else if (AClass == NLS_DCOMPLEX)
        {
            switch (BClass)
            {
                    OPCASE(NLS_INT32, 6);
                    OPCASE(NLS_SINGLE, 3);
                    OPCASE(NLS_DOUBLE, 3);
                    OPCASE(NLS_SCOMPLEX, 5);
                    OPCASE(NLS_DCOMPLEX, 5);
            }
        }
        else if (AClass == NLS_DOUBLE && Anegative)
        {
            switch (BClass)
            {
                    OPCASE(NLS_INT32, 7);
                    OPCASE(NLS_SINGLE, 5);
                    OPCASE(NLS_DOUBLE, 5);
                    OPCASE(NLS_SCOMPLEX, 5);
                    OPCASE(NLS_DCOMPLEX, 5);
            }
        }
        else if (AClass == NLS_DOUBLE && (!Anegative))
        {
            switch (BClass)
            {
                    OPCASE(NLS_INT32, 7);
                    OPCASE(NLS_SINGLE, 8);
                    OPCASE(NLS_DOUBLE, 8);
                    OPCASE(NLS_SCOMPLEX, 5);
                    OPCASE(NLS_DCOMPLEX, 5);
            }
        }
        else if (AClass == NLS_SINGLE && Anegative)
        {
            switch (BClass)
            {
                    OPCASE(NLS_INT32, 9);
                    OPCASE(NLS_SINGLE, 4);
                    OPCASE(NLS_DOUBLE, 5);
                    OPCASE(NLS_SCOMPLEX, 4);
                    OPCASE(NLS_DCOMPLEX, 5);
            }
        }
        else if (AClass == NLS_SINGLE && (!Anegative))
        {
            switch (BClass)
            {
                    OPCASE(NLS_INT32, 9);
                    OPCASE(NLS_SINGLE, 10);
                    OPCASE(NLS_DOUBLE, 8);
                    OPCASE(NLS_SCOMPLEX, 4);
                    OPCASE(NLS_DCOMPLEX, 5);
            }
        }
        // Invoke the appropriate case
        switch (opType)
        {
                MAPOP(1, NLS_SCOMPLEX, NLS_INT32, NLS_SCOMPLEX, (vvfun)cicpower);
                MAPOP(2, NLS_SCOMPLEX, NLS_SINGLE, NLS_SCOMPLEX, (vvfun)cfcpower);
                MAPOP(3, NLS_DCOMPLEX, NLS_DOUBLE, NLS_DCOMPLEX, (vvfun)zdzpower);
                MAPOP(4, NLS_SCOMPLEX, NLS_SCOMPLEX, NLS_SCOMPLEX, (vvfun)cccpower);
                MAPOP(5, NLS_DCOMPLEX, NLS_DCOMPLEX, NLS_DCOMPLEX, (vvfun)zzzpower);
                MAPOP(6, NLS_DCOMPLEX, NLS_INT32, NLS_DCOMPLEX, (vvfun)zizpower);
                MAPOP(7, NLS_DOUBLE, NLS_INT32, NLS_DOUBLE, (vvfun)didpower);
                MAPOP(8, NLS_DOUBLE, NLS_DOUBLE, NLS_DOUBLE, (vvfun)dddpower);
                MAPOP(9, NLS_SINGLE, NLS_INT32, NLS_SINGLE, (vvfun)fifpower);
                MAPOP(10, NLS_SINGLE, NLS_SINGLE, NLS_SINGLE, (vvfun)fffpower);
        }
        return ArrayOf();
    }
    //=============================================================================
    ArrayOf DotPower(ArrayOf A, ArrayOf B)
    {
        return(DoPowerTwoArgFunction(A, B));
    }
    //=============================================================================
}
//=============================================================================
