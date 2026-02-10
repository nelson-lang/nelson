//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "Evaluator.hpp"
#include "ValidatorsInternal.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
#include "IsValidVariableName.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
invalidPositionMessage(int argPosition)
{
    std::wstring msg;
    if (argPosition > 0) {
        msg = fmt::format(_W("Invalid input argument at position {}."), argPosition) + L"\n";
    }
    return msg;
}
//=============================================================================
static ArrayOfVector
evaluateFunction(const ArrayOfVector& args, int nLhs, const std::string& functionName)
{
    Evaluator* _eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    FunctionDef* fptr = nullptr;
    bool found = _eval->getContext()->lookupFunction(functionName, fptr);
    ArrayOfVector argOut;
    if (found) {
        argOut = fptr->evaluateFunction(_eval, args, nLhs);
    }
    if (argOut.size() < 1) {
        raiseError(
            L"Nelson:validators:ERROR_WRONG_NUMBERS_OUTPUT_ARGS", ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    return argOut;
}
//=============================================================================
void
mustBeLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].getContentAsLogicalScalar()) {
        bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
        if (!isLogical) {
            std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be logical.");
            Error(msg, L"Nelson:validators:ERROR_MUST_BE_LOGICAL");
        }
    }
}
//=============================================================================
void
mustBeFinite(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool asLogicalIsAllFinite = false;
    ArrayOfVector argIn(arg);
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be finite.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_FINITE");
    }
}
//=============================================================================
void
mustBeNonempty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(arg, 1, "isempty");
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be empty.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONEMPTY");
    }
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (isLogical) {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = evaluateFunction(argIn, 1, "isscalar");
        if (argOut[0].getContentAsLogicalScalar()) {
            return;
        }
    }
    std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be logical scalar.");
    Error(msg, L"Nelson:validators:ERROR_MUST_BE_LOGICAL_SCALAR");
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].getContentAsLogicalScalar()) {
        argOut = evaluateFunction(argIn, 1, "isscalar");
        if (!argOut[0].getContentAsLogicalScalar()) {
            std::wstring msg
                = invalidPositionMessage(argPosition) + _W("Value must be scalar or empty.");
            Error(msg, L"Nelson:validators:ERROR_MUST_BE_SCALAR_OR_EMPTY");
        }
    }
}
//=============================================================================
void
mustBeValidVariableName(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isvarname = false;
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        isvarname = IsValidVariableName(arg.getContentAsWideString());
    }
    if (!isvarname) {
        std::wstring msg
            = invalidPositionMessage(argPosition) + _W("Value must be valid variable name.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_VALID_VARIABLE_NAME");
    }
}
//=============================================================================
void
mustBeText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray()
        || arg.isCellArrayOfCharacterVectors();
    if (!isText) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be a character vector, string array or cell array of character "
                 "vectors.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_TEXT");
    }
}
//=============================================================================
void
mustBeTextScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isTextScalar = arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
    if (!isTextScalar) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be a character vector or string scalar.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_TEXT_SCALAR");
    }
}
//=============================================================================
void
mustBeFolder(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isdir");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be folder.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_FOLDER");
    }
}
//=============================================================================
void
mustBeFile(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isfile");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be file.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_FILE");
    }
}
//=============================================================================
void
mustBeVector(const ArrayOf& arg, bool allowsAllEmpties, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isvector");
    bool isVectorOrEmpty = argOut[0].getContentAsLogicalScalar();

    if (!isVectorOrEmpty && allowsAllEmpties) {
        argOut = evaluateFunction(argIn, 1, "isempty");
        isVectorOrEmpty = argOut[0].getContentAsLogicalScalar();
    }
    if (!isVectorOrEmpty) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a vector.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_VECTOR");
    }
}
//=============================================================================
void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isfloat");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a float.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_FLOAT");
    }
}
//=============================================================================
void
mustBeMatrix(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "ismatrix");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a matrix.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_MATRIX");
    }
}
//=============================================================================
void
mustBeRow(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isrow");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a row vector.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_ROW");
    }
}
//=============================================================================
void
mustBeColumn(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "iscolumn");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg
            = invalidPositionMessage(argPosition) + _W("Value must be a column vector.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_COLUMN");
    }
}
//=============================================================================
void
mustBeSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "issparse");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg
            = invalidPositionMessage(argPosition) + _W("Value must be a sparse matrix.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_SPARSE");
    }
}
//=============================================================================
void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be numeric.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC");
    }
}
//=============================================================================
void
mustBeA(const ArrayOf& arg, const wstringVector& classNames, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    std::wstring currentClassName;
    ClassName(arg, currentClassName);
    bool findClass = false;
    for (const auto& className : classNames) {
        if (currentClassName == className) {
            findClass = true;
        }
    }
    if (!findClass) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be one of the following types:") + L" ";
        std::wstring concatClass;
        for (size_t k = 0; k < classNames.size(); ++k) {
            if (k == 0) {
                concatClass = L"'" + classNames[k] + L"'";
            } else if (k == classNames.size() - 1) {
                concatClass = concatClass + L", " + _W("or") + L" " + L"'" + classNames[k] + L"'";
            } else {
                concatClass = concatClass + L", " + L"'" + classNames[k] + L"'";
            }
            if (k == classNames.size() - 1) {
                concatClass = concatClass + L".";
            }
        }
        msg = msg + concatClass;
        std::wstring id = L"Nelson:validators:mustBeA";
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_A");
    }
}
//=============================================================================
void
mustBePositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = L"Nelson:validators:mustBeReal";
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, GT_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isPositive = argOut[0].getContentAsLogicalScalar();
    if (!isPositive) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be positive.");
        std::wstring id = L"Nelson:validators:mustBePositive";
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_POSITIVE");
    }
}
//=============================================================================
void
mustBeNonpositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, LE_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isNonpositive = argOut[0].getContentAsLogicalScalar();
    if (!isNonpositive) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be non positive.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONPOSITIVE");
    }
}
//=============================================================================
void
mustBeNonnegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, GE_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isNonnegative = argOut[0].getContentAsLogicalScalar();
    if (!isNonnegative) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be nonnegative.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONNEGATIVE");
    }
}
//=============================================================================
void
mustBeNegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, LT_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isNegative = argOut[0].getContentAsLogicalScalar();
    if (!isNegative) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be negative.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NEGATIVE");
    }
}
//=============================================================================
void
mustBeNonNan(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].getContentAsLogicalScalar()) {
        Dimensions dimsA = argIn[0].getDimensions();
        Dimensions dimsV(1, dimsA.getElementCount());
        ArrayOf asVector = argIn[0];
        asVector.reshape(dimsV);
        argOut = evaluateFunction(asVector, 1, "isnan");
        argOut = evaluateFunction(argOut, 1, "any");
        bool isNaN = argOut[0].getContentAsLogicalScalar();
        if (isNaN) {
            std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be NaN.");
            Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONNAN");
        }
    }
}
//=============================================================================
void
mustBeNonZero(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    ArrayOfVector argOut = evaluateFunction(vAsArrayOfVector, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "any");
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be zero.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONZERO");
    }
}
//=============================================================================
void
mustBeNonSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "issparse");
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be sparse.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONSparse");
    }
}
//=============================================================================
void
mustBeReal(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
}
//=============================================================================
void
mustBeInteger(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be integer.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_INTEGER");
    }
    argOut = evaluateFunction(asVector, 1, "floor");
    argOut.push_back(asVector);
    argOut = evaluateFunction(argOut, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be integer.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_INTEGER");
    }
}
//=============================================================================
void
mustBeNonmissing(const ArrayOf& arg, int argPosition, bool asCaller)
{
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "ismissing");
    argOut = evaluateFunction(argOut[0], 1, "any");
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be non missing.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONMISSING");
    }
}
//=============================================================================
std::wstring
createPrintableScalar(const ArrayOf& c, const std::wstring& fmt, const std::wstring& defaultMessage)
{
    std::wstring valueAsString;
    switch (c.getDataClass()) {
    case NLS_LOGICAL: {
        if (c.getContentAsLogicalScalar()) {
            valueAsString = L"true";
        } else {
            valueAsString = L"false";
        }
    } break;
    case NLS_UINT8: {
        uint8 value = ArrayOf(c).getContentAsUnsignedInteger8Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_INT8: {
        int8 value = ArrayOf(c).getContentAsInteger8Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_UINT16: {
        uint16 value = ArrayOf(c).getContentAsUnsignedInteger16Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_INT16: {
        int16 value = ArrayOf(c).getContentAsInteger16Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_UINT32: {
        uint32 value = ArrayOf(c).getContentAsUnsignedInteger32Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_INT32: {
        int32 value = ArrayOf(c).getContentAsInteger32Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_UINT64: {
        uint64 value = ArrayOf(c).getContentAsUnsignedInteger64Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_INT64: {
        int64 value = ArrayOf(c).getContentAsInteger64Scalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_SINGLE: {
        single value = ArrayOf(c).getContentAsSingleScalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    case NLS_DOUBLE: {
        double value = ArrayOf(c).getContentAsDoubleScalar();
        valueAsString = fmt::to_wstring(value);
    } break;
    default: {
    } break;
    }
    std::wstring msg;
    if (valueAsString.empty()) {
        msg = defaultMessage;
    } else {
        msg = fmt::format(fmt, valueAsString);
    }
    return msg;
}
//=============================================================================
static bool
mustBeGreaterThan(const ArrayOf& arg, const ArrayOf& c)
{
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector params(asVector);
    params.push_back(c);
    ArrayOfVector argOut = evaluateFunction(params, 1, GT_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeGreaterThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isscalar");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Second input to function 'mustBeGreaterThan' must be a scalar.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_SCALAR_OR_EMPTY");
    }
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Inputs to function 'mustBeGreaterThan' must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeGreaterThan' must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    if (!mustBeGreaterThan(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be greater than {}."),
                _W("Value must be greater than compared value."));
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_GREATER_THAN");
    }
}
//=============================================================================
static bool
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c)
{
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector params(asVector);
    params.push_back(c);
    ArrayOfVector argOut = evaluateFunction(params, 1, LT_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = evaluateFunction(argC, 1, "isscalar");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Second input to function 'mustBeLessThan' must be a scalar.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_SCALAR_OR_EMPTY");
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Inputs to function 'mustBeLessThan' must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    ArrayOfVector argIn(arg);
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    argOut = evaluateFunction(argC, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeLessThan' must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    if (!mustBeLessThan(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be less than {}."),
                _W("Value must be less than compared value."));
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_LESS_THAN");
    }
}
//=============================================================================
static bool
mustBeGreaterThanOrEqual(const ArrayOf& arg, const ArrayOf& c)
{
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector params(asVector);
    params.push_back(c);
    ArrayOfVector argOut = evaluateFunction(params, 1, GE_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeGreaterThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = evaluateFunction(argC, 1, "isscalar");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg
            = _W("Second input to function 'mustBeGreaterThanOrEqual' must be a scalar.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_SCALAR_OR_EMPTY");
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg
            = _W("Inputs to function 'mustBeGreaterThanOrEqual' must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    ArrayOfVector argIn(arg);
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    argOut = evaluateFunction(argC, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeGreaterThanOrEqual' must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    if (!mustBeGreaterThanOrEqual(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be greater than or equal to {}."),
                _W("Value must be greater than or equal to compared value."));
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_GREATER_THAN_OR_EQUAL");
    }
}
//=============================================================================
static bool
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c)
{
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector params(asVector);
    params.push_back(c);
    ArrayOfVector argOut = evaluateFunction(params, 1, LE_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = evaluateFunction(argC, 1, "isscalar");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Second input to function 'mustBeLessThanOrEqual' must be a scalar.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_SCALAR_OR_EMPTY");
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg
            = _W("Inputs to function 'mustBeLessThanOrEqual' must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    ArrayOfVector argIn(arg);
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
    argOut = evaluateFunction(argC, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeLessThanOrEqual' must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_REAL");
    }
    if (!mustBeLessThanOrEqual(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be less than or equal to {}."),
                _W("Value must be less than or equal to compared value."));
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_LESS_THAN_OR_EQUAL");
    }
}
//=============================================================================
void
mustBeNumericOrLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg
            = invalidPositionMessage(argPosition) + _W("Value must be numeric or logical.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NUMERIC_OR_LOGICAL");
    }
}
//=============================================================================
void
mustBeNonzeroLengthText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray()
        || arg.isCellArrayOfCharacterVectors();
    if (!isText) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be a character vector, string array or cell array of character "
                 "vectors.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONZERO_LENGTH_TEXT");
    } else {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = evaluateFunction(argIn, 1, "strlength");
        argOut.push_back(ArrayOf::doubleConstructor(0));
        argOut = evaluateFunction(argOut, 1, GT_OPERATOR_STR);
        argOut.push_back(ArrayOf::characterArrayConstructor("all"));
        argOut = evaluateFunction(argOut, 1, "all");
        if (!argOut[0].getContentAsLogicalScalar()) {
            std::wstring msg
                = invalidPositionMessage(argPosition) + _W("Value must be non zero length text.");
            Error(msg, L"Nelson:validators:ERROR_MUST_BE_NONZERO_LENGTH_TEXT");
        }
    }
}
//=============================================================================
void
mustBeMember(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    argIn << c;
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "ismember");
    Dimensions dims = argOut[0].getDimensions();
    Dimensions dimsV(1, dims.getElementCount());
    ArrayOf asVector = argOut[0];
    asVector.reshape(dimsV);
    argOut = evaluateFunction(asVector, 1, "any");
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be member of the compared value.");
        Error(msg, L"Nelson:validators:ERROR_MUST_BE_MEMBER");
    }
}
//=============================================================================
void
mustBeInRange(const ArrayOf& value, const ArrayOf& lower, const ArrayOf& upper,
    const std::wstring& boundflag1, const std::wstring& boundflag2, int argPosition, bool asCaller)
{
    bool includeLower = false;
    bool includeUpper = false;

    if (boundflag1 == boundflag2 && boundflag1 == L"") {
        includeLower = true;
        includeUpper = true;
    } else if (boundflag1 == L"inclusive" && boundflag2 == L"") {
        includeLower = true;
        includeUpper = true;
    } else if (boundflag1 == L"exclusive" && boundflag2 == L"") {
        includeLower = false;
        includeUpper = false;
    } else if (boundflag1 == L"exclude-lower" && boundflag2 == L"") {
        includeLower = false;
        includeUpper = true;
    } else if (boundflag1 == L"exclude-upper" && boundflag2 == L"") {
        includeLower = true;
        includeUpper = false;
    } else if (boundflag1 == L"exclude-lower" && boundflag2 == L"exclude-upper") {
        includeLower = false;
        includeUpper = false;
    } else {
        if (boundflag1 == boundflag2) {
            std::wstring msg
                = fmt::format(_W("A combinaison of '{}' and '{}' options is not supported."),
                    boundflag1, boundflag2);
            raiseError(L"Nelson:validators:ERROR_INVALID_INPUT_POSITION_WITH_MSG",
                ERROR_INVALID_INPUT_POSITION_WITH_MSG, msg);
        } else {
            std::wstring msg
                = _W("Value must 'inclusive', 'exclusive', 'exclude-lower' or 'exclude-upper'.");
            raiseError(L"Nelson:validators:ERROR_INVALID_INPUT_POSITION_WITH_MSG",
                ERROR_INVALID_INPUT_POSITION_WITH_MSG, msg);
        }
    }
    mustBeNumericOrLogical(value, 1, asCaller);
    mustBeReal(value, 1, asCaller);

    ArrayOfVector argIn(lower);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isnumeric = argOut[0].getContentAsLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isreal");
    bool isreal = argOut[0].getContentAsLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isscalar");
    bool isscalar = argOut[0].getContentAsLogicalScalar();
    bool isLogical = (argOut[0].isLogical() || ClassName(argOut[0]) == "logical");
    if (!((isnumeric && isreal) || isLogical) || !isscalar) {
        std::wstring msg
            = _W("Second input to function 'mustBeInRange' must be a real or scalar value.");
        Error(msg, L"Nelson:validators:ERROR_INVALID_INPUT_POSITION_WITH_MSG");
    }

    argIn = upper;
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isnumeric = argOut[0].getContentAsLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isreal");
    isreal = argOut[0].getContentAsLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isscalar");
    isscalar = argOut[0].getContentAsLogicalScalar();
    isLogical = (argOut[0].isLogical() || ClassName(argOut[0]) == "logical");
    if (!((isnumeric && isreal) || isLogical) || !isscalar) {
        std::wstring msg
            = _W("Third input to function 'mustBeInRange' must be a real or scalar value.");
        Error(msg, L"Nelson:validators:ERROR_INVALID_INPUT_POSITION_WITH_MSG");
    }
    std::wstring id;
    if (includeLower) {
        if (includeUpper) {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = L"Nelson:validators:LeftClosedRightClosed";
        } else {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = L"Nelson:validators:LeftClosedRightOpen";
        }
    } else {
        if (includeUpper) {
            if (mustBeGreaterThan(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = L"Nelson:validators:LeftOpenRightClosed";
        } else {
            if (mustBeGreaterThan(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = L"Nelson:validators:LeftOpenRightOpen";
        }
    }
    std::wstring msg = _W("Value must be in range.");
    Error(msg, L"Nelson:validators:ERROR_INVALID_INPUT_POSITION_WITH_MSG");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
