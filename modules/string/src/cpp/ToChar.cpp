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
#include "ToChar.hpp"
#include "IEEEFP.hpp"
#include "IsCellOfStrings.hpp"
#include "VertCat.hpp"
#include <boost/container/vector.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ArrayOfDoubleToChar(const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    std::wstring res;
    res.reserve(A.getLength());
    double* pDouble = (double*)A.getDataPointer();
#if defined(__NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (indexType k = 0; k < A.getLength(); k++) {
        double v = pDouble[k];
        if (IsFinite(v)) {
            if (v < 0) {
                res.push_back((charType)0);
            } else {
                if (v > 65535) {
                    res.push_back((charType)65535);
                } else {
                    res.push_back((charType)(v));
                }
            }
        } else {
            if (IsNaN(v)) {
                res.push_back((charType)0);
            } else {
                if (v <= 0) {
                    res.push_back((charType)0);
                } else if (IsInfinite(v)) {
                    res.push_back((charType)65535);
                }
            }
        }
    }
    ArrayOf result = ArrayOf::characterArrayConstructor(res);
    result.reshape(dimsA);
    return result;
}
//=============================================================================
ArrayOf
ToChar(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    ArrayOf res;
    ArrayOf charA = ToChar(A, needToOverload);
    if (needToOverload) {
        return res;
    }
    ArrayOf charB = ToChar(B, needToOverload);
    if (needToOverload) {
        return res;
    }
    Dimensions dimsA = charA.getDimensions();
    Dimensions dimsB = charB.getDimensions();
    wstringVector vA = charA.getContentAsWideStringVector();
    if (vA.size() == 0) {
        vA.push_back(L"");
    }
    wstringVector vB = charB.getContentAsWideStringVector();
    if (vB.size() == 0) {
        vB.push_back(L"");
    }
    size_t lenMax = 0;
    for (indexType i = 0; i < vA.size(); ++i) {
        if (lenMax < vA[i].size()) {
            lenMax = vA[i].size();
        }
    }
    for (indexType i = 0; i < vB.size(); ++i) {
        if (lenMax < vB[i].size()) {
            lenMax = vB[i].size();
        }
    }
    for (indexType i = 0; i < vA.size(); ++i) {
        size_t newLen = lenMax - vA[i].size();
        if (newLen > 0) {
            for (size_t q = 0; q < newLen; q++) {
                vA[i] = vA[i] + std::wstring(L" ");
            }
        }
    }
    for (indexType i = 0; i < vB.size(); ++i) {
        size_t newLen = lenMax - vB[i].size();
        if (newLen > 0) {
            for (size_t q = 0; q < newLen; q++) {
                vB[i] = vB[i] + std::wstring(L" ");
            }
        }
    }
    res = ArrayOf::characterArrayConstructor(vA[0]);
    bool bSuccess;
    for (size_t i = 1; i < vA.size(); i++) {
        ArrayOf BA = ArrayOf::characterArrayConstructor(vA[i]);
        res = VertCat(res, BA, true, bSuccess);
    }
    for (size_t i = 0; i < vB.size(); i++) {
        ArrayOf BB = ArrayOf::characterArrayConstructor(vB[i]);
        res = VertCat(res, BB, true, bSuccess);
    }
    return res;
}
//=============================================================================
ArrayOf
ToChar(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        ArrayOfVector V;
        ArrayOf* arg = (ArrayOf*)(A.getDataPointer());
        for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
            ArrayOf val = ToChar(arg[k], needToOverload);
            if (needToOverload) {
                return res;
            }
            V.push_back(val);
        }
        res = V[0];
        for (indexType k = 1; k < V.size(); k++) {
            ArrayOf val = ToChar(res, V[k], needToOverload);
            if (needToOverload) {
                return res;
            }
            res = val;
        }
    } break;
    case NLS_CHAR: {
        ArrayOf R(A);
        R.ensureSingleOwner();
        return R;
    } break;
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE: {
        ArrayOf AA = A;
        AA.promoteType(NLS_DOUBLE);
        res = ArrayOfDoubleToChar(AA);
    } break;
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
static std::wstring
ToChar(wstringVector V, boost::container::vector<Dimensions> dimsVector, Dimensions& dims)
{
    std::wstring res;
    size_t lenMax = 0;
    for (size_t k = 0; k < V.size(); ++k) {
        if (lenMax < V[k].size()) {
            lenMax = dimsVector[k].getColumns();
        }
    }
    if (V.size() == 1) {
        dims = dimsVector[0];
        return V[0];
    } else {
        ArrayOf resAsArrayOf = ArrayOf::characterArrayConstructor(V[0]);
        resAsArrayOf.reshape(dimsVector[0]);
        bool bSuccess;
        for (size_t i = 1; i < V.size(); i++) {
            ArrayOf B = ArrayOf::characterArrayConstructor(V[i]);
            B.reshape(dimsVector[i]), resAsArrayOf = VertCat(resAsArrayOf, B, true, bSuccess);
        }
        res = resAsArrayOf.getContentAsArrayOfCharacters();
        dims[0] = resAsArrayOf.getDimensions().getRows();
        dims[1] = resAsArrayOf.getDimensions().getColumns();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================