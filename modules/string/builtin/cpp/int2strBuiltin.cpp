//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <vector>
#include "StringHelpers.hpp"
#include "int2strBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "IntegerToString.hpp"
#include "VertCat.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <Eigen/Sparse>
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
integerValueToString(double dvalue)
{
    if (std::isnan(dvalue)) {
        return L"NaN";
    }
    if (dvalue > 0) {
        if (std::isinf(dvalue)) {
            return L"Inf";
        }
        return std::to_wstring(static_cast<uint64>(std::round(dvalue)));
    }
    if (std::isinf(dvalue)) {
        return L"-Inf";
    }
    return std::to_wstring(static_cast<int64>(std::round(dvalue)));
}
//=============================================================================
template <class T>
static void
sparseToIntegerStringVector(const ArrayOf& A, wstringVector& result)
{
    result.assign(A.elementCount(), L"0");
    const auto* spmat
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    for (indexType k = 0; k < (indexType)spmat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spmat, k); it;
            ++it) {
            result[it.row() + it.col() * A.rows()] = integerValueToString((double)it.value());
        }
    }
}
//=============================================================================
static void
sparseComplexToIntegerStringVector(const ArrayOf& A, wstringVector& result)
{
    result.assign(A.elementCount(), L"0");
    const auto* spmat = static_cast<const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*>(
        A.sparseDataPointer());
    for (indexType k = 0; k < (indexType)spmat->outerSize(); ++k) {
        for (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>::InnerIterator it(*spmat, k);
            it; ++it) {
            result[it.row() + it.col() * A.rows()] = integerValueToString(it.value().real());
        }
    }
}
//=============================================================================
static bool
sparseIntegerToString(const ArrayOf& A, wstringVector& result)
{
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        sparseToIntegerStringVector<logical>(A, result);
        return true;
    case NLS_DOUBLE:
        sparseToIntegerStringVector<double>(A, result);
        return true;
    case NLS_DCOMPLEX:
        sparseComplexToIntegerStringVector(A, result);
        return true;
    default:
        return false;
    }
}
//=============================================================================
static ArrayOf
StringVectorToString(wstringVector V, Dimensions& DimsV)
{
    ArrayOf strArr;
    if (V.size() == 0) {
        Dimensions dims(1, 0);
        strArr = ArrayOf::empty(dims);
        strArr.promoteType(NLS_CHAR);
    } else {
        if (V.size() == 1) {
            strArr = ArrayOf::characterArray(V[0]);
        } else {
            size_t R = DimsV[0];
            size_t C = DimsV.elementCount() / DimsV[0];
            // Column widths are measured on the absolute value (a leading minus
            // sign is not counted; it borrows one of the two separating
            // spaces). A single row shares one global width across all
            // elements, a matrix uses a per-column maximum. Each element is
            // then right-justified in a field of width+2 and the common leading
            // whitespace is trimmed from every row.
            auto absLen = [](const std::wstring& s) -> size_t {
                return (!s.empty() && s[0] == L'-') ? s.size() - 1 : s.size();
            };
            std::vector<size_t> colWidth(C, 0);
            if (R == 1) {
                size_t w = 0;
                for (auto& k : V) {
                    w = std::max(w, absLen(k));
                }
                for (size_t c = 0; c < C; c++) {
                    colWidth[c] = w;
                }
            } else {
                for (size_t c = 0; c < C; c++) {
                    size_t w = 0;
                    for (size_t r = 0; r < R; r++) {
                        w = std::max(w, absLen(V[r + c * R]));
                    }
                    colWidth[c] = w;
                }
            }
            std::vector<std::wstring> lines(R);
            size_t minLead = std::wstring::npos;
            for (size_t r = 0; r < R; r++) {
                std::wstring line;
                for (size_t c = 0; c < C; c++) {
                    const std::wstring& val = V[r + c * R];
                    size_t field = colWidth[c] + 2;
                    size_t pad = (field > val.size()) ? (field - val.size()) : 0;
                    line.append(pad, L' ');
                    line += val;
                }
                size_t lead = line.find_first_not_of(L' ');
                if (lead == std::wstring::npos) {
                    lead = line.size();
                }
                minLead = std::min(minLead, lead);
                lines[r] = line;
            }
            if (minLead == std::wstring::npos) {
                minLead = 0;
            }
            // Trim the shared leading whitespace from each row and build the R-by-width
            // character matrix in one pass. All rows have the same length, so the stack is
            // exact.
            wstringVector trimmed;
            trimmed.reserve(R);
            for (size_t r = 0; r < R; r++) {
                trimmed.push_back(lines[r].substr(minLead));
            }
            strArr = ArrayOf::characterArrayFromRows(trimmed, false);
        }
    }
    return strArr;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::int2strBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    wstringVector result;
    std::wstring error_message;
    bool bRes = false;
    if (argIn[0].isSparse()) {
        bRes = sparseIntegerToString(argIn[0], result);
        if (!bRes) {
            error_message = _W("Type not supported.");
        }
    } else {
        bRes = IntegerToString(argIn[0], result, error_message);
    }
    if (bRes) {
        Dimensions dims = argIn[0].dimensions();
        retval << StringVectorToString(result, dims);
    } else {
        Error(L"Nelson:int2str:typeNotSupported", error_message);
    }
    return retval;
}
//=============================================================================
