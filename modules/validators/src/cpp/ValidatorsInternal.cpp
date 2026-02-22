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
        raiseErrorAsCaller(true, _E("nelson:arguments:wrongNumberOfOutputs"));
    }
    return argOut;
}
//=============================================================================
static void
raiseValidatorError(bool asCaller, const std::wstring& baseErrorId, int argPosition,
    const std::wstring& extraArg = L"")
{
    if (argPosition > 0) {
        std::wstring errorIdWithPosition = baseErrorId + L"AtPosition";
        if (extraArg.empty()) {
            raiseErrorAsCaller(asCaller, errorIdWithPosition, argPosition);
        } else {
            raiseErrorAsCaller(asCaller, errorIdWithPosition, argPosition, extraArg);
        }
    } else {
        if (extraArg.empty()) {
            raiseErrorAsCaller(asCaller, baseErrorId);
        } else {
            raiseErrorAsCaller(asCaller, baseErrorId, extraArg);
        }
    }
}
//=============================================================================
static ArrayOf
reshapeToVector(const ArrayOf& arg)
{
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    return asVector;
}
//=============================================================================
static void
validateWithFunction(const ArrayOf& arg, const std::string& functionName,
    const std::wstring& errorId, int argPosition, bool asCaller, bool expectTrue = true)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, functionName);
    bool result = argOut[0].getContentAsLogicalScalar();
    if (result != expectTrue) {
        raiseValidatorError(asCaller, errorId, argPosition);
    }
}
//=============================================================================
static void
validateComparison(const ArrayOf& arg, const std::string& operatorStr, const std::wstring& errorId,
    int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    mustBeReal(arg, argPosition, asCaller);
    ArrayOf asVector = reshapeToVector(arg);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    ArrayOfVector argOut = evaluateFunction(vAsArrayOfVector, 1, operatorStr);
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        raiseValidatorError(asCaller, errorId, argPosition);
    }
}
//=============================================================================
static bool
compareWithScalar(const ArrayOf& arg, const ArrayOf& c, const std::string& operatorStr)
{
    ArrayOf asVector = reshapeToVector(arg);
    ArrayOfVector params(asVector);
    params.push_back(c);
    ArrayOfVector argOut = evaluateFunction(params, 1, operatorStr);
    argOut = evaluateFunction(argOut, 1, "all");
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
static void
validateComparisonArguments(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller,
    const std::wstring& functionName)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = evaluateFunction(argC, 1, "isscalar");
    if (!argOut[0].getContentAsLogicalScalar()) {
        raiseErrorAsCaller(
            asCaller, _E("nelson:validators:invalidInputPositionAtSecondPosition"), functionName);
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        raiseErrorAsCaller(
            asCaller, _E("nelson:validators:invalidInputPositionAtFirstPosition"), functionName);
    }
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    mustBeReal(c, 0, asCaller);
    mustBeReal(arg, argPosition, asCaller);
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
            _E("nelson:validators:mustBeLogicalAtPosition");
            raiseValidatorError(asCaller, _E("nelson:validators:mustBeLogical"), argPosition);
        }
    }
}
//=============================================================================
void
mustBeFinite(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOf asVector = reshapeToVector(arg);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        _E("nelson:validators:mustBeFiniteAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeFinite"), argPosition);
    }
}
//=============================================================================
void
mustBeNonempty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    validateWithFunction(
        arg, "isempty", _E("nelson:validators:mustBeNonempty"), argPosition, asCaller, false);
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
    _E("nelson:validators:mustBeLogicalScalarAtPosition");
    raiseValidatorError(asCaller, _E("nelson:validators:mustBeLogicalScalar"), argPosition);
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
            _E("nelson:validators:mustBeScalarOrEmptyAtPosition");
            raiseValidatorError(asCaller, _E("nelson:validators:mustBeScalarOrEmpty"), argPosition);
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
        _E("nelson:validators:mustBeValidVariableNameAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeValidVariableName"), argPosition);
    }
}
//=============================================================================
void
mustBeText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray()
        || arg.isCellArrayOfCharacterVectors();
    if (!isText) {
        _E("nelson:validators:mustBeTextAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeText"), argPosition);
    }
}
//=============================================================================
void
mustBeTextScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isTextScalar = arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
    if (!isTextScalar) {
        _E("nelson:validators:mustBeTextScalarAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeTextScalar"), argPosition);
    }
}
//=============================================================================
void
mustBeFolder(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeTextScalar(arg, argPosition, asCaller);
    _E("nelson:validators:mustBeFolderAtPosition");
    validateWithFunction(arg, "isdir", _E("nelson:validators:mustBeFolder"), argPosition, asCaller);
}
//=============================================================================
void
mustBeFile(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeTextScalar(arg, argPosition, asCaller);
    _E("nelson:validators:mustBeFileAtPosition");
    validateWithFunction(arg, "isfile", _E("nelson:validators:mustBeFile"), argPosition, asCaller);
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
        _E("nelson:validators:mustBeVectorAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeVector"), argPosition);
    }
}
//=============================================================================
void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeFloatAtPosition");
    validateWithFunction(
        arg, "isfloat", _E("nelson:validators:mustBeFloat"), argPosition, asCaller);
}
//=============================================================================
void
mustBeMatrix(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeMatrixAtPosition");
    validateWithFunction(
        arg, "ismatrix", _E("nelson:validators:mustBeMatrix"), argPosition, asCaller);
}
//=============================================================================
void
mustBeRow(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeRowAtPosition");
    validateWithFunction(arg, "isrow", _E("nelson:validators:mustBeRow"), argPosition, asCaller);
}
//=============================================================================
void
mustBeColumn(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeColumnAtPosition");
    validateWithFunction(
        arg, "iscolumn", _E("nelson:validators:mustBeColumn"), argPosition, asCaller);
}
//=============================================================================
void
mustBeSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeSparseAtPosition");
    validateWithFunction(
        arg, "issparse", _E("nelson:validators:mustBeSparse"), argPosition, asCaller);
}
//=============================================================================
void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller)
{
    validateWithFunction(
        arg, "isnumeric", _E("nelson:validators:mustBeNumeric"), argPosition, asCaller);
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
        std::wstring concatClass;
        for (size_t k = 0; k < classNames.size(); ++k) {
            if (k == 0) {
                concatClass = L"'" + classNames[k] + L"'";
            } else if (k == classNames.size() - 1) {
                concatClass = concatClass + L", " + _W("or") + L" " + L"'" + classNames[k] + L"'";
            } else {
                concatClass = concatClass + L", " + L"'" + classNames[k] + L"'";
            }
        }
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, _E("nelson:validators:mustBeAAtPosition"), argPosition, concatClass);
        } else {
            raiseErrorAsCaller(asCaller, _E("nelson:validators:mustBeA"), concatClass);
        }
    }
}
//=============================================================================
void
mustBePositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBePositiveAtPosition");
    validateComparison(
        arg, GT_OPERATOR_STR, _E("nelson:validators:mustBePositive"), argPosition, asCaller);
}
//=============================================================================
void
mustBeNonpositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeNonpositiveAtPosition");
    validateComparison(
        arg, LE_OPERATOR_STR, _E("nelson:validators:mustBeNonpositive"), argPosition, asCaller);
}
//=============================================================================
void
mustBeNonnegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeNonnegativeAtPosition");
    validateComparison(
        arg, GE_OPERATOR_STR, _E("nelson:validators:mustBeNonnegative"), argPosition, asCaller);
}
//=============================================================================
void
mustBeNegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeNegativeAtPosition");
    validateComparison(
        arg, LT_OPERATOR_STR, _E("nelson:validators:mustBeNegative"), argPosition, asCaller);
}
//=============================================================================
void
mustBeNonNan(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].getContentAsLogicalScalar()) {
        ArrayOf asVector = reshapeToVector(arg);
        argOut = evaluateFunction(asVector, 1, "isnan");
        argOut = evaluateFunction(argOut, 1, "any");
        bool isNaN = argOut[0].getContentAsLogicalScalar();
        if (isNaN) {
            _E("nelson:validators:mustBeNonNanAtPosition");
            raiseValidatorError(asCaller, _E("nelson:validators:mustBeNonNan"), argPosition);
        }
    }
}
//=============================================================================
void
mustBeNonZero(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOf asVector = reshapeToVector(arg);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    ArrayOfVector argOut = evaluateFunction(vAsArrayOfVector, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "any");
    if (argOut[0].getContentAsLogicalScalar()) {
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeNonZero"), argPosition);
    }
}
//=============================================================================
void
mustBeNonSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeNonSparseAtPosition");
    validateWithFunction(
        arg, "issparse", _E("nelson:validators:mustBeNonSparse"), argPosition, asCaller, false);
}
//=============================================================================
void
mustBeReal(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeRealAtPosition");
    validateWithFunction(arg, "isreal", _E("nelson:validators:mustBeReal"), argPosition, asCaller);
}
//=============================================================================
void
mustBeInteger(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    mustBeReal(arg, argPosition, asCaller);
    ArrayOf asVector = reshapeToVector(arg);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        _E("nelson:validators:mustBeIntegerAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeInteger"), argPosition);
    }
    argOut = evaluateFunction(asVector, 1, "floor");
    argOut.push_back(asVector);
    argOut = evaluateFunction(argOut, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeInteger"), argPosition);
    }
}
//=============================================================================
void
mustBeNonmissing(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOf asVector = reshapeToVector(arg);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "ismissing");
    argOut = evaluateFunction(argOut[0], 1, "any");
    if (argOut[0].getContentAsLogicalScalar()) {
        _E("nelson:validators:mustBeNonMissingAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeNonMissing"), argPosition);
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
            valueAsString = TRUE_STR;
        } else {
            valueAsString = FALSE_STR;
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
    return compareWithScalar(arg, c, GT_OPERATOR_STR);
}
//=============================================================================
void
mustBeGreaterThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeGreaterThanAtPosition");
    validateComparisonArguments(arg, c, argPosition, asCaller, L"mustBeGreaterThan");
    if (!mustBeGreaterThan(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        raiseValidatorError(
            asCaller, _E("nelson:validators:mustBeGreaterThan"), argPosition, printable);
    }
}
//=============================================================================
static bool
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c)
{
    return compareWithScalar(arg, c, LT_OPERATOR_STR);
}
//=============================================================================
void
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeLessThanAtPosition");
    validateComparisonArguments(arg, c, argPosition, asCaller, L"mustBeLessThan");
    if (!mustBeLessThan(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        raiseValidatorError(
            asCaller, _E("nelson:validators:mustBeLessThan"), argPosition, printable);
    }
}
//=============================================================================
static bool
mustBeGreaterThanOrEqual(const ArrayOf& arg, const ArrayOf& c)
{
    return compareWithScalar(arg, c, GE_OPERATOR_STR);
}
//=============================================================================
void
mustBeGreaterThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    validateComparisonArguments(arg, c, argPosition, asCaller, L"mustBeGreaterThanOrEqual");
    if (!mustBeGreaterThanOrEqual(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        // workaround to include generated error message in translation files, since the error
        // message is generated dynamically based on the operator
        _E("nelson:validators:mustBeGreaterThanOrEqualAtPosition");
        raiseValidatorError(
            asCaller, _E("nelson:validators:mustBeGreaterThanOrEqual"), argPosition, printable);
    }
}
//=============================================================================
static bool
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c)
{
    return compareWithScalar(arg, c, LE_OPERATOR_STR);
}
//=============================================================================
void
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeLessThanOrEqualAtPosition");
    validateComparisonArguments(arg, c, argPosition, asCaller, L"mustBeLessThanOrEqual");
    if (!mustBeLessThanOrEqual(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        raiseValidatorError(
            asCaller, _E("nelson:validators:mustBeLessThanOrEqual"), argPosition, printable);
    }
}
//=============================================================================
void
mustBeNumericOrLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeNumericOrLogicalAtPosition");
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeNumericOrLogical"), argPosition);
    }
}
//=============================================================================
void
mustBeNonzeroLengthText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    _E("nelson:validators:mustBeNonzeroLengthTextAtPosition");
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray()
        || arg.isCellArrayOfCharacterVectors();
    if (!isText) {
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeNonzeroLengthText"), argPosition);
    } else {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = evaluateFunction(argIn, 1, "strlength");
        argOut.push_back(ArrayOf::doubleConstructor(0));
        argOut = evaluateFunction(argOut, 1, GT_OPERATOR_STR);
        argOut.push_back(ArrayOf::characterArrayConstructor("all"));
        argOut = evaluateFunction(argOut, 1, "all");
        if (!argOut[0].getContentAsLogicalScalar()) {
            raiseValidatorError(
                asCaller, _E("nelson:validators:mustBeNonzeroLengthText"), argPosition);
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
    ArrayOf asVector = reshapeToVector(argOut[0]);
    argOut = evaluateFunction(asVector, 1, "any");
    if (!argOut[0].getContentAsLogicalScalar()) {
        _E("nelson:validators:mustBeMemberAtPosition");
        raiseValidatorError(asCaller, _E("nelson:validators:mustBeMember"), argPosition);
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
            raiseErrorAsCaller(
                asCaller, _E("nelson:validators:unsupportedCombination"), boundflag1, boundflag2);
        } else {
            raiseErrorAsCaller(asCaller, _E("nelson:validators:invalidBoundFlag"));
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
        raiseErrorAsCaller(asCaller, _E("nelson:validators:invalidInputPositionAtSecondPosition"),
            L"mustBeInRange");
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
        raiseErrorAsCaller(asCaller, _E("nelson:validators:invalidInputPositionAtThirdPosition"),
            L"mustBeInRange");
    }
    std::wstring id;
    if (includeLower) {
        if (includeUpper) {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = _E("nelson:validators:leftClosedRightClosed");
        } else {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = _E("nelson:validators:leftClosedRightOpen");
            raiseErrorAsCaller(asCaller, id);
        }
    } else {
        if (includeUpper) {
            if (mustBeGreaterThan(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = _E("nelson:validators:leftOpenRightClosed");
        } else {
            if (mustBeGreaterThan(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = _E("nelson:validators:leftOpenRightOpen");
        }
    }
    raiseErrorAsCaller(asCaller, id);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
