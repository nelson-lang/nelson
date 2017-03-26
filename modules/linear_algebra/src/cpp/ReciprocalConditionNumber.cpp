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
#include <Eigen/Dense>
#include "lapack_eigen.hpp"
#include <Eigen/src/misc/lapacke.h>
#include "ReciprocalConditionNumber.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	static ArrayOf ReciprocalConditionNumber_Double(ArrayOf A)
	{
		ArrayOf rcond;
		Eigen::Map<Eigen::MatrixXd> matA((double*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
		if (matA.hasNaN())
		{
			rcond = ArrayOf::doubleConstructor(std::nan(""));
		}
		else
		{
			if (A.isScalar())
			{
				if (std::isinf(matA(0)))
				{
					rcond = ArrayOf::doubleConstructor(0.);
				}
				else
				{
					if (matA(0) == 0.)
					{
						rcond = ArrayOf::doubleConstructor(0.);
					}
					else
					{
						rcond = ArrayOf::doubleConstructor(1.);
					}
				}
			}
			else
			{
				double normA = 0;
				char norm = '1';
				int m = (int)A.getDimensions().getRows();
				int n = (int)A.getDimensions().getColumns();
				int lda = m;

				normA = LAPACKE_dlange(LAPACK_COL_MAJOR, norm, m, n, (const double*)A.getDataPointer(), lda);

				int info = 0;
				int *ipiv = new int[std::min(m, n)];
				if (ipiv == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}

				LAPACK_dgetrf(&m, &n, (double*)A.getDataPointer(), &lda, ipiv, &info);
				delete[] ipiv;
				ipiv = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_dgetrf error."));
				}
				info = 0;
				double *work = new double[4 * n];
				if (work == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				int *iwork = new int[n];
				if (iwork == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				double res = 0.;
				LAPACK_dgecon(&norm, &n, (double*)A.getDataPointer(), &lda, &normA, &res, work, iwork, &info);
				delete[] iwork;
				iwork = nullptr;
				delete[] work;
				work = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_dgecon error."));
				}
				rcond = ArrayOf::doubleConstructor(res);
			}
		}
		return rcond;
	}
	//=============================================================================
	static ArrayOf ReciprocalConditionNumber_DoubleComplex(ArrayOf A)
	{
		ArrayOf rcond;
		doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
		Eigen::Map<Eigen::MatrixXcd> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
		if (matA.hasNaN())
		{
			rcond = ArrayOf::doubleConstructor(std::nan(""));
		}
		else
		{
			if (A.isScalar())
			{
				doublecomplex dcplx = matA(0);
				if (std::isinf(dcplx.real()) || std::isinf(dcplx.imag()))
				{
					rcond = ArrayOf::doubleConstructor(0.);
				}
				else
				{
					if (dcplx.real() == 0. && dcplx.imag() == 0.)
					{
						rcond = ArrayOf::doubleConstructor(0.);
					}
					else
					{
						rcond = ArrayOf::doubleConstructor(1.);
					}
				}
			}
			else
			{
				double normA = 0;
				char norm = '1';
				int m = (int)A.getDimensions().getRows();
				int n = (int)A.getDimensions().getColumns();
				int lda = m;

				doublecomplex* Az = reinterpret_cast<doublecomplex*>((double*)A.getDataPointer());
				normA = LAPACKE_zlange(LAPACK_COL_MAJOR, norm, m, n, Az, lda);

				int info = 0;
				int *ipiv = new int[std::min(m, n)];
				if (ipiv == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}

				LAPACK_zgetrf(&m, &n, Az, &lda, ipiv, &info);
				delete[] ipiv;
				ipiv = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_zgetrf error."));
				}
				info = 0;
				doublecomplex  *work = new doublecomplex[4 * n];
				if (work == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				double *rwork = new double[2 * n];
				if (rwork == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				double res = 0.;
				LAPACK_zgecon(&norm, &n, Az, &lda, &normA, &res, work, rwork, &info);
				delete[] rwork;
				rwork = nullptr;
				delete[] work;
				work = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_zgecon error."));
				}
				rcond = ArrayOf::doubleConstructor(res);
			}
		}
		return rcond;
	}
	//=============================================================================
	static ArrayOf ReciprocalConditionNumber_Single(ArrayOf A)
	{
		ArrayOf rcond;
		Eigen::Map<Eigen::MatrixXf> matA((single*)A.getDataPointer(), (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
		if (matA.hasNaN())
		{
			rcond = ArrayOf::singleConstructor(std::nanf(""));
		}
		else
		{
			if (A.isScalar())
			{
				if (std::isinf(matA(0)))
				{
					rcond = ArrayOf::singleConstructor(0.);
				}
				else
				{
					if (matA(0) == 0.)
					{
						rcond = ArrayOf::singleConstructor(0.);
					}
					else
					{
						rcond = ArrayOf::singleConstructor(1.);
					}
				}
			}
			else
			{
				single normA = 0;
				char norm = '1';
				int m = (int)A.getDimensions().getRows();
				int n = (int)A.getDimensions().getColumns();
				int lda = m;

				normA = LAPACKE_slange(LAPACK_COL_MAJOR, norm, m, n, (const single*)A.getDataPointer(), lda);

				int info = 0;
				int *ipiv = new int[std::min(m, n)];
				if (ipiv == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}

				LAPACK_sgetrf(&m, &n, (single*)A.getDataPointer(), &lda, ipiv, &info);
				delete[] ipiv;
				ipiv = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_sgetrf error."));
				}
				info = 0;
				single *work = new single[4 * n];
				if (work == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				int *iwork = new int[n];
				if (iwork == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				single res = 0.;
				LAPACK_sgecon(&norm, &n, (single*)A.getDataPointer(), &lda, &normA, &res, work, iwork, &info);
				delete[] iwork;
				iwork = nullptr;
				delete[] work;
				work = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_sgecon error."));
				}
				rcond = ArrayOf::singleConstructor(res);
			}
		}
		return rcond;
	}
	//=============================================================================
	static ArrayOf ReciprocalConditionNumber_SingleComplex(ArrayOf A)
	{
		ArrayOf rcond;
		singlecomplex* Az = reinterpret_cast<singlecomplex*>((single*)A.getDataPointer());
		Eigen::Map<Eigen::MatrixXcf> matA(Az, (Eigen::Index)A.getDimensions().getRows(), (Eigen::Index)A.getDimensions().getColumns());
		if (matA.hasNaN())
		{
			rcond = ArrayOf::singleConstructor(std::nanf(""));
		}
		else
		{
			if (A.isScalar())
			{
				singlecomplex scplx = matA(0);
				if (std::isinf(scplx.real()) || std::isinf(scplx.imag()))
				{
					rcond = ArrayOf::singleConstructor(0.);
				}
				else
				{
					if (scplx.real() == 0. && scplx.imag() == 0.)
					{
						rcond = ArrayOf::singleConstructor(0.);
					}
					else
					{
						rcond = ArrayOf::singleConstructor(1.);
					}
				}
			}
			else
			{
				single normA = 0;
				char norm = '1';
				int m = (int)A.getDimensions().getRows();
				int n = (int)A.getDimensions().getColumns();
				int lda = m;
				normA = LAPACKE_clange(LAPACK_COL_MAJOR, norm, m, n, Az, lda);

				int info = 0;
				int *ipiv = new int[std::min(m, n)];
				if (ipiv == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}

				LAPACK_cgetrf(&m, &n, Az, &lda, ipiv, &info);
				delete[] ipiv;
				ipiv = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_cgetrf error."));
				}
				info = 0;
				singlecomplex  *work = new singlecomplex[4 * n];
				if (work == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				single *rwork = new single[2 * n];
				if (rwork == nullptr)
				{
					throw Exception(ERROR_MEMORY_ALLOCATION);
				}
				single res = 0.;
				LAPACK_cgecon(&norm, &n, Az, &lda, &normA, &res, work, rwork, &info);
				delete[] rwork;
				rwork = nullptr;
				delete[] work;
				work = nullptr;
				if (info < 0)
				{
					throw Exception(_("LAPACK_cgecon error."));
				}
				rcond = ArrayOf::singleConstructor(res);
			}
		}
		return rcond;
	}
	//=============================================================================
	ArrayOf ReciprocalConditionNumber(ArrayOf A)
	{
		ArrayOf rcond;
		bool isSupportedTypes = (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE ||
			A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX) && !A.isSparse();
		if (!isSupportedTypes)
		{
			throw Exception(_("Undefined function 'rcond' for input arguments of type") + " '" + ClassName(A) + "'.");
		}
		if (!A.isSquare())
		{
			throw Exception(_("Square matrix expected."));
		}
		if (A.isEmpty())
		{
			if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX)
			{
				rcond = ArrayOf::doubleConstructor(std::numeric_limits<double>::infinity());
			}
			else
			{
				rcond = ArrayOf::singleConstructor(std::numeric_limits<single>::infinity());
			}
		}
		else
		{
			if (A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_DCOMPLEX)
			{
				if (A.getDataClass() == NLS_DOUBLE)
				{
					rcond = ReciprocalConditionNumber_Double(A);
				}
				else
				{
					rcond = ReciprocalConditionNumber_DoubleComplex(A);
				}
			}
			else
			{
				if (A.getDataClass() == NLS_SINGLE)
				{
					rcond = ReciprocalConditionNumber_Single(A);
				}
				else
				{
					rcond = ReciprocalConditionNumber_SingleComplex(A);
				}
			}
		}
		return rcond;
	}
	//=============================================================================
}
//=============================================================================
