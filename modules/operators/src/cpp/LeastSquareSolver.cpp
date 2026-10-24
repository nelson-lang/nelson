//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LeastSquareSolver.hpp"
#include "LapackLeastSquaresSolver.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
template <class T>
ArrayOf
solveLeastSquareLapack(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB,
    std::wstring& warningId, std::string& warningMessage)
{
    indexType m = matA.dimensionLength(0);
    indexType n = matA.dimensionLength(1);
    warningId.clear();
    warningMessage.clear();

    lapack_int rank = 0;
    lapack_int info = 0;
    ArrayOf res = solveLapackLeastSquares<T>(destinationClass, matA, matB, false, rank, info);
    if (info != 0) {
        Error(L"Nelson:operators:solverFailed", L"Linear least squares solver failed.");
    }
    if (isRankDeficient<T>(m, n, rank)) {
        snprintf(msgBuffer, MSGBUFLEN, "%s RANK = %d",
            resolveMessageTemplate("Nelson:operators:matrixRankDeficientMachine",
                "Matrix is rank deficient to machine precision:")
                .c_str(),
            static_cast<int>(rank));
        warningMessage = msgBuffer;
        warningId = WARNING_RANK_DEFICIENT_MATRIX;
    }
    return res;
}
//=============================================================================
ArrayOf
solveLeastSquareDouble(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareLapack<double>(NLS_DOUBLE, matA, matB, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLeastSquareSingle(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareLapack<single>(NLS_SINGLE, matA, matB, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLeastSquareDoubleComplex(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareLapack<doublecomplex>(
        NLS_DCOMPLEX, matA, matB, warningId, warningMessage);
}
//=============================================================================
ArrayOf
solveLeastSquareSingleComplex(
    const ArrayOf& matA, const ArrayOf& matB, std::wstring& warningId, std::string& warningMessage)
{
    return solveLeastSquareLapack<singlecomplex>(
        NLS_SCOMPLEX, matA, matB, warningId, warningMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
