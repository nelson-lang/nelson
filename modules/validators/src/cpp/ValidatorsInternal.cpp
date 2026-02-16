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
        raiseErrorAsCaller(true, L"nelson:arguments:wrongNumberOfOutputs");
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
            if (argPosition > 0) {
                raiseErrorAsCaller(
                    asCaller, L"nelson:validators:mustBeLogicalAtPosition", argPosition);
            } else {
                raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeLogical");
            }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFiniteAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFinite");
        }
    }
}
//=============================================================================
void
mustBeNonempty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(arg, 1, "isempty");
    if (argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNonemptyAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonempty");
        }
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
    if (argPosition > 0) {
        raiseErrorAsCaller(
            asCaller, L"nelson:validators:mustBeLogicalScalarAtPosition", argPosition);
    } else {
        raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeLogicalScalar");
    }
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
            if (argPosition > 0) {
                raiseErrorAsCaller(
                    asCaller, L"nelson:validators:mustBeScalarOrEmptyAtPosition", argPosition);
            } else {
                raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeScalarOrEmpty");
            }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeValidVariableNameAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeValidVariableName");
        }
    }
}
//=============================================================================
void
mustBeText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray()
        || arg.isCellArrayOfCharacterVectors();
    if (!isText) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeTextAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeText");
        }
    }
}
//=============================================================================
void
mustBeTextScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isTextScalar = arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
    if (!isTextScalar) {
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeTextScalarAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeTextScalar");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFolderAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFolder");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFileAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFile");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeVectorAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeVector");
        }
    }
}
//=============================================================================
void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isfloat");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFloatAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeFloat");
        }
    }
}
//=============================================================================
void
mustBeMatrix(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "ismatrix");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeMatrixAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeMatrix");
        }
    }
}
//=============================================================================
void
mustBeRow(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isrow");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRowAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRow");
        }
    }
}
//=============================================================================
void
mustBeColumn(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "iscolumn");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeColumnAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeColumn");
        }
    }
}
//=============================================================================
void
mustBeSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "issparse");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeSparseAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeSparse");
        }
    }
}
//=============================================================================
void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isnumeric");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNumericAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNumeric", argPosition);
        }
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
                asCaller, L"nelson:validators:mustBeAAtPosition", argPosition, concatClass);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeA", concatClass);
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBePositiveAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBePositive");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNonpositiveAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonpositive");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNonnegativeAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonnegative");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNegativeAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNegative");
        }
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
            if (argPosition > 0) {
                raiseErrorAsCaller(
                    asCaller, L"nelson:validators:mustBeNonNanAtPosition", argPosition);
            } else {
                raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonNan");
            }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonZeroAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonZero");
        }
    }
}
//=============================================================================
void
mustBeNonSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "issparse");
    if (argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNonSparseAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonSparse");
        }
    }
}
//=============================================================================
void
mustBeReal(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = evaluateFunction(argIn, 1, "isreal");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    argOut = evaluateFunction(asVector, 1, "isfinite");
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeIntegerAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeInteger");
        }
    }
    argOut = evaluateFunction(asVector, 1, "floor");
    argOut.push_back(asVector);
    argOut = evaluateFunction(argOut, 1, EQ_OPERATOR_STR);
    argOut = evaluateFunction(argOut, 1, "all");
    if (!argOut[0].getContentAsLogicalScalar()) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeIntegerAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeInteger");
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNonmissingAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonmissing");
        }
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
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtSecondPosition",
            L"mustBeGreaterThan");
    }
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtFirstPosition",
            L"mustBeGreaterThan");
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    if (!mustBeGreaterThan(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeGreaterThanAtPosition", argPosition, printable);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeGreaterThan", printable);
        }
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
        raiseErrorAsCaller(
            asCaller, L"nelson:validators:InvalidInputPositionAtSecondPosition", L"mustBeLessThan");
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        raiseErrorAsCaller(
            asCaller, L"nelson:validators:InvalidInputPositionAtFirstPosition", L"mustBeLessThan");
    }
    ArrayOfVector argIn(arg);
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNumericOrLogicalAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNumericOrLogical");
        }
    }
    argOut = evaluateFunction(argC, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    if (!mustBeLessThan(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeLessThanAtPosition", argPosition, printable);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeLessThan", printable);
        }
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
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtSecondPosition",
            L"mustBeGreaterThanOrEqual");
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtFirstPosition",
            L"mustBeGreaterThanOrEqual");
    }
    ArrayOfVector argIn(arg);
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNumericOrLogicalAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNumericOrLogical");
        }
    }
    argOut = evaluateFunction(argC, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    if (!mustBeGreaterThanOrEqual(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeGreaterThanOrEqualAtPosition",
                argPosition, printable);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeGreaterThanOrEqual", printable);
        }
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
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtSecondPosition",
            L"mustBeLessThanOrEqual");
    }
    argOut = evaluateFunction(argC, 1, "isnumeric");
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtFirstPosition",
            L"mustBeLessThanOrEqual");
    }
    ArrayOfVector argIn(arg);
    argOut = evaluateFunction(argIn, 1, "isnumeric");
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNumericOrLogicalAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNumericOrLogical");
        }
    }
    argOut = evaluateFunction(argC, 1, "isreal");
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        raiseErrorAsCaller(asCaller, L"nelson:validators:InvalidInputPositionAtSecondPosition",
            L"mustBeLessThanOrEqual");
    }
    argOut = evaluateFunction(argIn, 1, "isreal");
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeRealAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeReal");
        }
    }
    if (!mustBeLessThanOrEqual(arg, c)) {
        std::wstring printable = createPrintableScalar(c, _W("{}"), _W(""));
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeLessThanOrEqualAtPosition",
                argPosition, printable);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeLessThanOrEqual", printable);
        }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNumericOrLogicalAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNumericOrLogical");
        }
    }
}
//=============================================================================
void
mustBeNonzeroLengthText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray()
        || arg.isCellArrayOfCharacterVectors();
    if (!isText) {
        if (argPosition > 0) {
            raiseErrorAsCaller(
                asCaller, L"nelson:validators:mustBeNonzeroLengthTextAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonzeroLengthText");
        }
    } else {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = evaluateFunction(argIn, 1, "strlength");
        argOut.push_back(ArrayOf::doubleConstructor(0));
        argOut = evaluateFunction(argOut, 1, GT_OPERATOR_STR);
        argOut.push_back(ArrayOf::characterArrayConstructor("all"));
        argOut = evaluateFunction(argOut, 1, "all");
        if (!argOut[0].getContentAsLogicalScalar()) {
            if (argPosition > 0) {
                raiseErrorAsCaller(
                    asCaller, L"nelson:validators:mustBeNonzeroLengthTextAtPosition", argPosition);
            } else {
                raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeNonzeroLengthText");
            }
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
        if (argPosition > 0) {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeMemberAtPosition", argPosition);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:mustBeMember");
        }
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
                asCaller, L"nelson:validators:unsupportedCombination", boundflag1, boundflag2);
        } else {
            raiseErrorAsCaller(asCaller, L"nelson:validators:invalidBoundFlag");
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
        raiseErrorAsCaller(
            asCaller, L"nelson:validators:invalidInputPositionAtSecondPosition", L"mustBeInRange");
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
        raiseErrorAsCaller(
            asCaller, L"nelson:validators:invalidInputPositionAtThirdPosition", L"mustBeInRange");
    }
    std::wstring id;
    if (includeLower) {
        if (includeUpper) {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = L"nelson:validators:leftClosedRightClosed";
        } else {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = L"nelson:validators:leftClosedRightOpen";
            raiseErrorAsCaller(asCaller, id);
        }
    } else {
        if (includeUpper) {
            if (mustBeGreaterThan(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = L"nelson:validators:leftOpenRightClosed";
        } else {
            if (mustBeGreaterThan(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = L"nelson:validators:leftOpenRightOpen";
        }
    }
    raiseErrorAsCaller(asCaller, id);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
