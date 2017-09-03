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
#include <algorithm>
#include "slicot_SB01BD.hpp"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
	extern int sb01bd_(char *dico, int *n, int *m, int *np, double *alpha, double *a, int *lda, double *b, int *ldb, double *wr, double *wi, int *nfp, int *nap, int *nup, double *f, int *ldf, double *z, int *ldz, double *tol, double *dwork, int *ldwork, int *iwarn, int *info);
#ifdef __cplusplus
}
#endif
//=============================================================================
namespace Nelson
{
	//=============================================================================
	ArrayOfVector slicot_SB01BD(const std::string &DICO, double ALPHA, ArrayOf A, ArrayOf B, ArrayOf WR, ArrayOf WI, double TOL)
	{
	// http://slicot.org/objects/software/shared/doc/SB01BD.html
		ArrayOfVector retval;
		char *dico = (char*)DICO.c_str();
		Dimensions dimsA = A.getDimensions();
		int n = (int)dimsA.getRows();
		int m = (int)dimsA.getColumns();
		Dimensions dimsWR = WR.getDimensions();
		int np = (int)(std::max(dimsWR.getRows(), dimsWR.getColumns()));
		int lda = (int)(std::max(1, n));
		int ldb = (int)(std::max(1, n));
		int ldf = (int)(std::max(1, m));
		int ldz = (int)(std::max(1, n));
		
		double *__DWORK = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, std::max(std::max(std::max(1, 5 * m), 5 * n), 2 * n + 4 * m));
		int ldwork = (int)(std::max(std::max(std::max(1, 5 * m), 5 * n), 2 * n + 4 * m));

		// output
		int nfp = 0;
		int nap = 0;
		int nup = 0;
		double *__F = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, std::max(1, m) * n);
		double *__Z = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, std::max(1, m) * n);
		int iwarn = 0;
		int info = 0;

		ArrayOf __WR = WR;
		__WR.ensureSingleOwner();

		ArrayOf __WI = WI;
		__WR.ensureSingleOwner();

		ArrayOf __A = A;
		__A.ensureSingleOwner();

		try
		{
			sb01bd_(dico, &n, &m, &np, &ALPHA, 
				(double*)__A.getDataPointer(), &lda, 
				(double*)B.getDataPointer(), &ldb, 
				(double*)__WR.getDataPointer(), (double*)__WI.getDataPointer(),
				&nfp, &nap, &nup,
				__F, &ldf,
				__Z, &ldz,
				&TOL, 
				__DWORK, &ldwork, 
				&iwarn, &info);
		}
		catch (const int)
		{
			delete[] __F;
			delete[] __Z;
		}

		delete[] __DWORK;

		Dimensions dimsF_out(ldf, n);
		ArrayOf F_out = ArrayOf::ArrayOf(NLS_DOUBLE, dimsF_out, __F);

		Dimensions dimsZ_out(ldf, n);
		ArrayOf Z_out = ArrayOf::ArrayOf(NLS_DOUBLE, dimsZ_out, __Z);

		retval.push_back(__A);
		retval.push_back(__WR);
		retval.push_back(__WI);
		retval.push_back(ArrayOf::doubleConstructor(nfp));
		retval.push_back(ArrayOf::doubleConstructor(nap));
		retval.push_back(ArrayOf::doubleConstructor(nup));
		retval.push_back(F_out);
		retval.push_back(Z_out);
		retval.push_back(ArrayOf::doubleConstructor(iwarn));
		retval.push_back(ArrayOf::doubleConstructor(info));

		return retval;
	}
	//=============================================================================
}
//=============================================================================
