//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qrBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "QRDecomposition.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct QROptions
{
    QRMode mode = QRMode::Full;
    QRPermutationOutput permutationOutput = QRPermutationOutput::Matrix;
    bool legacyZero = false;
};
//=============================================================================
bool
isTextOption(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
bool
isNumericZeroOption(const ArrayOf& arg)
{
    if (arg.isSparse() || arg.isComplex() || !arg.isNumeric() || !arg.isScalar()) {
        return false;
    }
    return arg.asScalarIndex(true) == 0;
}
//=============================================================================
void
parseQROption(const ArrayOf& arg, QROptions& options)
{
    if (isTextOption(arg)) {
        std::wstring text = arg.asWideString();
        if (text == L"econ") {
            options.mode = QRMode::Economy;
            return;
        }
        if (text == L"matrix") {
            options.permutationOutput = QRPermutationOutput::Matrix;
            return;
        }
        if (text == L"vector") {
            options.permutationOutput = QRPermutationOutput::Vector;
            return;
        }
        Error(L"Nelson:qr:invalidOption", L"Option must be 'econ', 'matrix', or 'vector'.");
    }
    if (isNumericZeroOption(arg)) {
        if (options.mode == QRMode::Economy
            || options.permutationOutput == QRPermutationOutput::Vector
            || options.permutationOutput == QRPermutationOutput::Matrix) {
            options.mode = QRMode::Economy;
            options.permutationOutput = QRPermutationOutput::Vector;
            options.legacyZero = true;
            return;
        }
    }
    Error(L"Nelson:qr:invalidOption:optionEconMatrixVector0",
        L"Option must be 'econ', 'matrix', 'vector', or 0.");
}
//=============================================================================
bool
isOptionCandidate(const ArrayOf& arg)
{
    return isTextOption(arg) || isNumericZeroOption(arg);
}
//=============================================================================
void
ensureSupportedMatrixType(const ArrayOf& A)
{
    if (!A.is2D()) {
        Error(L"Nelson:linear_algebra:matrix2DExpected", L"%s expected.", L"Vector or matrix 2D");
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX:
        return;
    default:
        OverloadRequired("qr");
    }
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::qrBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 3);
    ensureSupportedMatrixType(argIn[0]);

    QROptions options;
    bool hasRightHandSide = false;
    ArrayOf B;
    if (argIn.size() >= 2) {
        if (isOptionCandidate(argIn[1])) {
            parseQROption(argIn[1], options);
        } else {
            if (!argIn[0].isSparse()) {
                Error(L"Nelson:qr:rhsRequiresSparse",
                    L"Right-hand side matrix is only supported if the first input matrix is "
                    L"sparse.");
            }
            B = argIn[1];
            ensureSupportedMatrixType(B);
            if (B.rows() != argIn[0].rows()) {
                Error(L"Nelson:sizeDimensionsMustMatch",
                    L"Arrays have incompatible sizes for this operation.");
            }
            hasRightHandSide = true;
        }
    }
    if (argIn.size() == 3) {
        if (options.legacyZero) {
            Error(L"Nelson:qr:invalidOptionCombination",
                L"Option 0 must not be combined with other options. Use 'econ' instead.");
        }
        parseQROption(argIn[2], options);
        if (options.legacyZero
            && (options.mode == QRMode::Economy
                || options.permutationOutput == QRPermutationOutput::Vector)) {
            if (argIn[1].isRowVectorCharacterArray() || argIn[1].isStringArray()) {
                Error(L"Nelson:qr:invalidOptionCombination",
                    L"Option 0 must not be combined with other options. Use 'econ' instead.");
            }
        }
    }

    if (hasRightHandSide) {
        return QRSparseLeastSquares(argIn[0], B, nLhs, options.mode, options.permutationOutput);
    }
    return QRDecomposition(argIn[0], nLhs, options.mode, options.permutationOutput);
}
//=============================================================================
