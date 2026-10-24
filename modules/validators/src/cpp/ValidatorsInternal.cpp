//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "ValidatorsInternal.hpp"
#include "ValidatorsHelpers.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonConfiguration.hpp"
#include "IsValidVariableName.hpp"
#include "IsBetween.hpp"
#include "Operators.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
invalidPositionMessage(int argPosition)
{
    std::wstring msg;
    if (argPosition > 0) {
        msg = formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:invalidInputPosition",
                                    L"Invalid input argument at position %d."),
                  argPosition)
            + L"\n";
    }
    return msg;
}
//=============================================================================
ArrayOfVector
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
        Error(L"Nelson:notEnoughOutputs", L"Not enough output arguments.");
    }
    return argOut;
}
//=============================================================================
void
mustBeLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].asLogicalScalar()) {
        bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
        if (!isLogical) {
            std::wstring msg = invalidPositionMessage(argPosition)
                + formatI18nMessage(resolveMessageTemplate(
                                        L"Nelson:validators:mustBeLogical", L"%s must be logical."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:mustBeLogical", asCaller);
        }
    }
}
//=============================================================================
void
mustBeFinite(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool asLogicalIsAllFinite = false;
    ArrayOfVector argIn(arg);
    Dimensions dimsA = argIn[0].dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeFinite", L"%s must be finite."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeFinite", asCaller);
    }
}
//=============================================================================
void
mustBeNonempty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(arg, 1, "isempty");
    if (argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBeNonempty", L"%s must be nonempty."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonempty", asCaller);
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
        if (argOut[0].asLogicalScalar()) {
            return;
        }
    }
    std::wstring msg = invalidPositionMessage(argPosition)
        + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeLogicalScalar",
                                L"%s must be a logical scalar."),
            L"Value");
    RaiseError(msg, L"Nelson:validators:mustBeLogicalScalar", asCaller);
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].asLogicalScalar()) {
        argOut = evaluateFunction(argIn, 1, "isscalar");
        if (!argOut[0].asLogicalScalar()) {
            std::wstring msg = invalidPositionMessage(argPosition)
                + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeScalarOrEmpty",
                                        L"%s must be a scalar or empty."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:mustBeScalarOrEmpty", asCaller);
        }
    }
}
//=============================================================================
void
mustBeScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isscalar");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeScalar", L"%s must be a scalar."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeScalar", asCaller);
    }
}
//=============================================================================
void
mustBeValidVariableName(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isvarname = false;
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        isvarname = IsValidVariableName(arg.asWideString());
    }
    if (!isvarname) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeValidVariableName",
                                    L"%s must be a valid variable name."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeValidVariableName", asCaller);
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
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeText",
                    L"%s must contain text: a character vector, string array, or cell array of "
                    L"character vectors."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeText", asCaller);
    }
}
//=============================================================================
void
mustBeTextScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isTextScalar = arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
    if (!isTextScalar) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeTextScalar",
                                    L"%s must be a character vector or string scalar."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeTextScalar", asCaller);
    }
}
//=============================================================================
void
mustBeFolder(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isdir");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeFolder", L"%s must be a folder."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeFolder", asCaller);
    }
}
//=============================================================================
void
mustBeFile(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isfile");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeFile", L"%s must be a file."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeFile", asCaller);
    }
}
//=============================================================================
void
mustBeVector(const ArrayOf& arg, bool allowsAllEmpties, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isvector");
    bool isVectorOrEmpty = argOut[0].asLogicalScalar();

    if (!isVectorOrEmpty && allowsAllEmpties) {
        argOut = evaluateFunction(argIn, 1, "isempty");
        isVectorOrEmpty = argOut[0].asLogicalScalar();
    }
    if (!isVectorOrEmpty) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeVector", L"%s must be a vector."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeVector", asCaller);
    }
}
//=============================================================================
void
mustBeVectorOrEmpty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isvector");
    bool isVectorOrEmpty = argOut[0].asLogicalScalar();

    if (!isVectorOrEmpty) {
        argOut = evaluateFunction(argIn, 1, "isempty");
        isVectorOrEmpty = argOut[0].asLogicalScalar();
    }
    if (!isVectorOrEmpty) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeVectorOrEmpty",
                                    L"%s must be a 1-by-n vector, an n-by-1 vector, or empty."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeVectorOrEmpty", asCaller);
    }
}
//=============================================================================
void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isfloat");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeFloat", L"%s must be a float."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeFloat", asCaller);
    }
}
//=============================================================================
void
mustBeMatrix(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "ismatrix");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBeMatrix", L"%s must be a 2-D matrix."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeMatrix", asCaller);
    }
}
//=============================================================================
void
mustBeRow(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isrow");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeRow", L"%s must be a row vector."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeRow", asCaller);
    }
}
//=============================================================================
void
mustBeColumn(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "iscolumn");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeColumn",
                                    L"%s must be a column vector."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeColumn", asCaller);
    }
}
//=============================================================================
void
mustBeSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "issparse");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeSparse",
                                    L"%s must be a sparse matrix."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeSparse", asCaller);
    }
}
//=============================================================================
void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeNumeric", L"%s must be numeric."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNumeric", asCaller);
    }
}
//=============================================================================
void
mustBePositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].asLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeReal", L"%s must be real."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeReal", asCaller);
    }
    Dimensions dimsA = argIn[0].dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleScalar(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, GT_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isPositive = argOut[0].asLogicalScalar();
    if (!isPositive) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBePositive", L"%s must be positive."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBePositive", asCaller);
    }
}
//=============================================================================
void
mustBeNonpositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].asLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeReal", L"%s must be real."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeReal", asCaller);
    }
    Dimensions dimsA = argIn[0].dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleScalar(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, LE_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isNonpositive = argOut[0].asLogicalScalar();
    if (!isNonpositive) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeNonpositive",
                                    L"%s must be nonpositive."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonpositive", asCaller);
    }
}
//=============================================================================
void
mustBeNonnegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].asLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeReal", L"%s must be real."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeReal", asCaller);
    }
    Dimensions dimsA = argIn[0].dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleScalar(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, GE_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isNonnegative = argOut[0].asLogicalScalar();
    if (!isNonnegative) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeNonnegative",
                                    L"%s must be nonnegative."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonnegative", asCaller);
    }
}
//=============================================================================
void
mustBeNegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].asLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeReal", L"%s must be real."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeReal", asCaller);
    }
    Dimensions dimsA = argIn[0].dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleScalar(0));
    argOut = evaluateFunction(vAsArrayOfVector, 1, LT_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    bool isNegative = argOut[0].asLogicalScalar();
    if (!isNegative) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBeNegative", L"%s must be negative."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNegative", asCaller);
    }
}
//=============================================================================
void
mustBeNonNan(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isempty");
    if (!argOut[0].asLogicalScalar()) {
        Dimensions dimsA = argIn[0].dimensions();
        Dimensions dimsV(1, dimsA.elementCount());
        ArrayOf asVector = argIn[0];
        asVector.reshape(dimsV);
        argOut = evaluateFunction(asVector, 1, "isnan");
        argOut = evaluateFunction(argOut, 1, "any");
        bool isNaN = argOut[0].asLogicalScalar();
        if (isNaN) {
            std::wstring msg = invalidPositionMessage(argPosition)
                + formatI18nMessage(resolveMessageTemplate(
                                        L"Nelson:validators:mustBeNonNan", L"%s must not be NaN."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:mustBeNonNan", asCaller);
        }
    }
}
//=============================================================================
void
mustBeNonZero(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    Dimensions dimsA = arg.dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleScalar(0));
    ArrayOfVector argOut = evaluateFunction(vAsArrayOfVector, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "any");
    if (argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeNonZero", L"%s must be nonzero."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonZero", asCaller);
    }
}
//=============================================================================
void
mustBeNonSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "issparse");
    if (argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBeNonSparse", L"%s must be nonsparse."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonSparse", asCaller);
    }
}
//=============================================================================
void
mustBeReal(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeReal", L"%s must be real."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeReal", asCaller);
    }
}
//=============================================================================
void
mustBeInteger(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].asLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeReal", L"%s must be real."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeReal", asCaller);
    }
    Dimensions dimsA = arg.dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBeInteger", L"%s must be an integer."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeInteger", asCaller);
    }
    argOut = evaluateFunction(asVector, 1, "floor");
    argOut.push_back(asVector);
    argOut = evaluateFunction(argOut, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(
                                    L"Nelson:validators:mustBeInteger", L"%s must be an integer."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeInteger", asCaller);
    }
}
//=============================================================================
void
mustBeNonmissing(const ArrayOf& arg, int argPosition, bool asCaller)
{
    Dimensions dimsA = arg.dimensions();
    Dimensions dimsV(1, dimsA.elementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector argOut = evaluateFunction(asVector, 1, "ismissing");
    argOut = evaluateFunction(argOut[0], 1, "any");
    if (argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeNonmissing",
                                    L"%s must be nonmissing."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonmissing", asCaller);
    }
}
//=============================================================================
void
mustBeNumericOrLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    bool isNumeric = argOut[0].asLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeNumericOrLogical",
                                    L"%s must be numeric or logical."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNumericOrLogical", asCaller);
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
            + formatI18nMessage(
                resolveMessageTemplate(L"Nelson:validators:mustBeNonzeroLengthText",
                    L"%s must contain text: a character vector, string array, or cell array of "
                    L"character vectors."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeNonzeroLengthText", asCaller);
    } else {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = evaluateFunction(argIn, 1, "strlength");
        argOut.push_back(ArrayOf::doubleScalar(0));
        argOut = evaluateFunction(argOut, 1, GT_OPERATOR_STR);
        argOut.push_back(ArrayOf::characterArray("all"));
        argOut = evaluateFunction(argOut, 1, "all");
        if (!argOut[0].asLogicalScalar()) {
            std::wstring msg = invalidPositionMessage(argPosition)
                + formatI18nMessage(
                    resolveMessageTemplate(L"Nelson:validators:mustBeNonzeroLengthText:zeroLength",
                        L"%s must be non zero length text."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:mustBeNonzeroLengthText:zeroLength", asCaller);
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
    Dimensions dims = argOut[0].dimensions();
    Dimensions dimsV(1, dims.elementCount());
    ArrayOf asVector = argOut[0];
    asVector.reshape(dimsV);
    argOut = evaluateFunction(asVector, 1, "all");
    if (!argOut[0].asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeMember",
                                    L"%s must be member of the compared value."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeMember", asCaller);
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
            ErrorAsCaller(L"Nelson:validatorUsage:ConflictingBoundaryOptions",
                L"A combination of '%s' and '%s' options is not supported.", boundflag1,
                boundflag2);
        } else {
            std::wstring msg = resolveMessageTemplate(L"Nelson:validators:BoundInclusivity",
                L"Value must 'inclusive', 'exclusive', 'exclude-lower' or 'exclude-upper'.");
            RaiseError(msg, L"Nelson:validators:BoundInclusivity", asCaller);
        }
    }
    mustBeNumericOrLogical(value, 1, asCaller);
    mustBeReal(value, 1, asCaller);

    ArrayOfVector argIn(lower);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isnumeric = argOut[0].asLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isreal");
    bool isreal = argOut[0].asLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isscalar");
    bool isscalar = argOut[0].asLogicalScalar();
    bool isLogical = (argOut[0].isLogical() || ClassName(argOut[0]) == "logical");
    if (!((isnumeric && isreal) || isLogical) || !isscalar) {
        std::wstring msg = resolveMessageTemplate(L"Nelson:validatorUsage:invalidLowerBound",
            L"Second input to function 'mustBeInRange' must be a real or scalar value.");
        RaiseError(msg, L"Nelson:validatorUsage:invalidLowerBound", asCaller);
    }

    argIn = upper;
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isnumeric = argOut[0].asLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isreal");
    isreal = argOut[0].asLogicalScalar();
    argOut = evaluateFunction(argIn, 1, "isscalar");
    isscalar = argOut[0].asLogicalScalar();
    isLogical = (argOut[0].isLogical() || ClassName(argOut[0]) == "logical");
    if (!((isnumeric && isreal) || isLogical) || !isscalar) {
        std::wstring msg = resolveMessageTemplate(L"Nelson:validatorUsage:invalidUpperBound",
            L"Third input to function 'mustBeInRange' must be a real or scalar value.");
        RaiseError(msg, L"Nelson:validatorUsage:invalidUpperBound", asCaller);
    }
    if (includeLower) {
        if (includeUpper) {
            if (allCompare(value, lower, GE_OPERATOR_STR)
                && allCompare(value, upper, LE_OPERATOR_STR)) {
                return;
            }
            std::wstring msg = formatI18nMessage(
                resolveMessageTemplate(
                    L"Nelson:validators:LeftClosedRightClosed", L"%s must be in range."),
                L"Value");
            RaiseError(msg, L"Nelson:validators:LeftClosedRightClosed", asCaller);
        } else {
            if (allCompare(value, lower, GE_OPERATOR_STR)
                && allCompare(value, upper, LT_OPERATOR_STR)) {
                return;
            }
            std::wstring msg
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:LeftClosedRightOpen",
                                        L"%s must be in range."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:LeftClosedRightOpen", asCaller);
        }
    } else {
        if (includeUpper) {
            if (allCompare(value, lower, GT_OPERATOR_STR)
                && allCompare(value, upper, LE_OPERATOR_STR)) {
                return;
            }
            std::wstring msg
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:LeftOpenRightClosed",
                                        L"%s must be in range."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:LeftOpenRightClosed", asCaller);
        } else {
            if (allCompare(value, lower, GT_OPERATOR_STR)
                && allCompare(value, upper, LT_OPERATOR_STR)) {
                return;
            }
            std::wstring msg
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:LeftOpenRightOpen",
                                        L"%s must be in range."),
                    L"Value");
            RaiseError(msg, L"Nelson:validators:LeftOpenRightOpen", asCaller);
        }
    }
}
//=============================================================================
void
mustBeBetween(const ArrayOfVector& args, int argPosition, bool asCaller)
{
    Evaluator* _eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    ArrayOf result = AllBetween(_eval, args);
    if (!result.asLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + formatI18nMessage(resolveMessageTemplate(L"Nelson:validators:mustBeBetween",
                                    L"%s must be within the range specified by lower and upper."),
                L"Value");
        RaiseError(msg, L"Nelson:validators:mustBeBetween", asCaller);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
