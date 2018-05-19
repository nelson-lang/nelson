//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "UnaryMinus.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	template <class Tin, class Tout>
	void uminusReal(indexType N, const Tin *A, Tout* C)
	{
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
		for (indexType i = 0; i < N; i++)
		{
			C[i] = -A[i];
		}
	}
	//=============================================================================
	template <class Tin, class Tout>
	void uminusComplex(indexType N, const Tin *A, Tout* C)
	{
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
		for (indexType i = 0; i < N * 2; i++)
		{
			C[i] = -A[i];
			C[i + 1] = -A[i + 1];
		}
	}
	//=============================================================================
	ArrayOf UnaryMinus(ArrayOf &A, bool mustRaiseError, bool &bSuccess)
	{
		bSuccess = false;
		if (A.isSparse())
		{
			if (mustRaiseError)
			{
				std::string overload = ClassName(A) + "_uminus";
				throw Exception(_("function") + " " + overload + " " + _("undefined."));
			}
			else
			{
				return ArrayOf();
			}
		}
		void *ptrA = (void*)A.getDataPointer();
		indexType N = A.getDimensions().getElementCount();
		Class outputClass = A.getDataClass();
		void *Cp = nullptr;
		switch (A.getDataClass())
		{
		case NLS_LOGICAL:
		{
			outputClass = NLS_DOUBLE;
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<logical, double>(N, (logical*)ptrA, (double*)Cp);
		}
		break;
		case NLS_UINT8:
		case NLS_UINT16:
		case NLS_UINT32:
		case NLS_UINT64:
		{
			Cp = ArrayOf::allocateArrayOf(A.getDataClass(), A.getDimensions().getElementCount());
		}
		break;
		case NLS_INT8:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<int8, int8>(N, (int8*)ptrA, (int8*)Cp);
		}
		break;
		case NLS_INT16:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<int16, int16>(N, (int16*)ptrA, (int16*)Cp);
		}
		break;
		case NLS_INT32:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<int32, int32>(N, (int32*)ptrA, (int32*)Cp);
		}
		break;
		case NLS_INT64:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<int64, int64>(N, (int64*)ptrA, (int64*)Cp);
		}
		break;
		case NLS_SINGLE:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<single, single>(N, (single*)ptrA, (single*)Cp);
		}
		break;
		case NLS_DOUBLE:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<double, double>(N, (double*)ptrA, (double*)Cp);
		}
		break;
		case NLS_SCOMPLEX:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusComplex<single, single>(N, (single*)ptrA, (single*)Cp);
		}
		break;
		case NLS_DCOMPLEX:
		{
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusComplex<double, double>(N, (double*)ptrA, (double*)Cp);
		}
		break;
		case NLS_CHAR:
		{
			outputClass = NLS_DOUBLE;
			Cp = ArrayOf::allocateArrayOf(outputClass, N);
			uminusReal<charType, double>(N, (charType*)ptrA, (double*)Cp);
		}
		break;
		default:
		{
			if (mustRaiseError)
			{
				std::string overload = ClassName(A) + "_uminus";
				throw Exception(_("function") + " " + overload + " " + _("undefined."));
			}
			else
			{
				bSuccess = false;
				return ArrayOf();
			}
		}
		break;
		}
		bSuccess = true;
		return ArrayOf(outputClass, A.getDimensions(), Cp);
	}
}
//=============================================================================
