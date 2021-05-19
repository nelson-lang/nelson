//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/format.hpp>
#include "ValidatorsInternal.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "isfiniteBuiltin.hpp"
#include "isemptyBuiltin.hpp"
#include "isscalarBuiltin.hpp"
#include "IsValidVariableName.hpp"
#include "isdirBuiltin.hpp"
#include "isfileBuiltin.hpp"
#include "isvectorBuiltin.hpp"
#include "isfloatBuiltin.hpp"
#include "isnumericBuiltin.hpp"
#include "isrealBuiltin.hpp"
#include "isnanBuiltin.hpp"
#include "issparseBuiltin.hpp"
#include "isrealBuiltin.hpp"
#include "floorBuiltin.hpp"
#include "ismissingBuiltin.hpp"
#include "IsCellOfStrings.hpp"
#include "strlengthBuiltin.hpp"
#include "ismemberBuiltin.hpp"
#include "gtBuiltin.hpp"
#include "geBuiltin.hpp"
#include "ltBuiltin.hpp"
#include "leBuiltin.hpp"
#include "eqBuiltin.hpp"
#include "allBuiltin.hpp"
#include "anyBuiltin.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Evaluator* _eval = nullptr;
//=============================================================================
void
setEvaluator(Evaluator* eval)
{
    _eval = eval;
}
//=============================================================================
Evaluator*
getEvaluator()
{
    return _eval;
}
//=============================================================================
static std::wstring
invalidPositionMessage(int argPosition)
{
    std::wstring msg;
    if (argPosition > 0) {
        msg = str(boost::wformat { _W("Invalid input argument at position %d.") } % argPosition)
            + L"\n";
    }
    return msg;
}
//=============================================================================
void
mustBeLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
        if (!isLogical) {
            std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be logical.");
            std::wstring id = _W("Nelson:validators:mustBeLogical");
            Error(msg, id, asCaller);
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
    ArrayOfVector argOut = ElementaryFunctionsGateway::isfiniteBuiltin(_eval, 1, asVector);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be finite.");
        std::wstring id = _W("Nelson:validators:mustBeFinite");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNonempty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, arg);
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be empty.");
        std::wstring id = _W("Nelson:validators:mustBeNonempty");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeLogicalScalar(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (isLogical) {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
        if (argOut[0].getContentAsLogicalScalar()) {
            return;
        }
    }
    std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be logical scalar.");
    std::wstring id = _W("Nelson:validators:mustBeLogicalScalar");
    Error(msg, id, asCaller);
}
//=============================================================================
void
mustBeScalarOrEmpty(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
        if (!argOut[0].getContentAsLogicalScalar()) {
            std::wstring msg
                = invalidPositionMessage(argPosition) + _W("Value must be scalar or empty.");
            std::wstring id = _W("Nelson:validators:mustBeScalarOrEmpty");
            Error(msg, id, asCaller);
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
        std::wstring id = _W("Nelson:validators:mustBeValidVariableName");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray() || IsCellOfString(arg);
    if (!isText) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be a character vector, string array or cell array of character "
                 "vectors.");
        std::wstring id = _W("Nelson:validators:mustBeText");
        Error(msg, id, asCaller);
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
        std::wstring id = _W("Nelson:validators:mustBeTextScalar");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeFolder(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = FilesFoldersGateway::isdirBuiltin(1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be folder.");
        std::wstring id = _W("Nelson:validators:mustBeFolder");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeFile(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    mustBeTextScalar(arg, argPosition, asCaller);
    ArrayOfVector argOut = FilesFoldersGateway::isfileBuiltin(1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be file.");
        std::wstring id = _W("Nelson:validators:mustBeFile");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeVector(const ArrayOf& arg, bool allowsAllEmpties, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isvectorBuiltin(_eval, 1, argIn);
    bool isVectorOrEmpty = argOut[0].getContentAsLogicalScalar();

    if (!isVectorOrEmpty && allowsAllEmpties) {
        argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
        isVectorOrEmpty = argOut[0].getContentAsLogicalScalar();
    }
    if (!isVectorOrEmpty) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a vector.");
        std::wstring id = _W("Nelson:validators:mustBeVector");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeFloat(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isfloatBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be a float.");
        std::wstring id = _W("Nelson:validators:mustBeFloat");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNumeric(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be numeric.");
        std::wstring id = _W("Nelson:validators:mustBeNumeric");
        Error(msg, id, asCaller);
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
    for (size_t k = 0; k < classNames.size(); ++k) {
        if (currentClassName == classNames[k]) {
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
        std::wstring id = _W("Nelson:validators:mustBeA");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBePositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = OperatorsGateway::gtBuiltin(_eval, 1, vAsArrayOfVector);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    bool isPositive = argOut[0].getContentAsLogicalScalar();
    if (!isPositive) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be positive.");
        std::wstring id = _W("Nelson:validators:mustBePositive");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNonpositive(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = OperatorsGateway::leBuiltin(_eval, 1, vAsArrayOfVector);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    bool isNonpositive = argOut[0].getContentAsLogicalScalar();
    if (!isNonpositive) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be non positive.");
        std::wstring id = _W("Nelson:validators:mustBeNonpositive");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNonnegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = OperatorsGateway::geBuiltin(_eval, 1, vAsArrayOfVector);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    bool isNonnegative = argOut[0].getContentAsLogicalScalar();
    if (!isNonnegative) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be nonnegative.");
        std::wstring id = _W("Nelson:validators:mustBeNonnegative");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNegative(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    Dimensions dimsA = argIn[0].getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = argIn[0];
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    vAsArrayOfVector.push_back(ArrayOf::doubleConstructor(0));
    argOut = OperatorsGateway::ltBuiltin(_eval, 1, vAsArrayOfVector);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    bool isNegative = argOut[0].getContentAsLogicalScalar();
    if (!isNegative) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be negative.");
        std::wstring id = _W("Nelson:validators:mustBeNegative");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNonNan(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isemptyBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        Dimensions dimsA = argIn[0].getDimensions();
        Dimensions dimsV(1, dimsA.getElementCount());
        ArrayOf asVector = argIn[0];
        asVector.reshape(dimsV);
        ArrayOfVector argOut = ElementaryFunctionsGateway::isnanBuiltin(_eval, 1, asVector);
        argOut = OperatorsGateway::anyBuiltin(_eval, 1, argOut);
        bool isNaN = argOut[0].getContentAsLogicalScalar();
        if (isNaN) {
            std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be NaN.");
            std::wstring id = _W("Nelson:validators:mustBeNonNan");
            Error(msg, id, asCaller);
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
    ArrayOfVector argOut = OperatorsGateway::eqBuiltin(_eval, 1, vAsArrayOfVector);
    argOut = OperatorsGateway::anyBuiltin(_eval, 1, argOut);
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be zero.");
        std::wstring id = _W("Nelson:validators:mustBeNonZero");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNonSparse(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::issparseBuiltin(_eval, 1, argIn);
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must not be sparse.");
        std::wstring id = _W("Nelson:validators:mustBeNonSparse");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeReal(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeInteger(const ArrayOf& arg, int argPosition, bool asCaller)
{
    mustBeNumericOrLogical(arg, argPosition, asCaller);
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    Dimensions dimsA = arg.getDimensions();
    Dimensions dimsV(1, dimsA.getElementCount());
    ArrayOf asVector = arg;
    asVector.reshape(dimsV);
    ArrayOfVector vAsArrayOfVector(asVector);
    argOut = ElementaryFunctionsGateway::isfiniteBuiltin(_eval, 1, asVector);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be integer.");
        std::wstring id = _W("Nelson:validators:mustBeInteger");
        Error(msg, id, asCaller);
    }
    argOut = ElementaryFunctionsGateway::floorBuiltin(_eval, 1, asVector);
    argOut.push_back(asVector);
    argOut = OperatorsGateway::eqBuiltin(_eval, 1, argOut);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be integer.");
        std::wstring id = _W("Nelson:validators:mustBeInteger");
        Error(msg, id, asCaller);
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
    ArrayOfVector argOut = ElementaryFunctionsGateway::ismissingBuiltin(_eval, 1, asVector);
    argOut = OperatorsGateway::anyBuiltin(_eval, 1, argOut[0]);
    if (argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition) + _W("Value must be non missing.");
        std::wstring id = _W("Nelson:validators:mustBeNonmissing");
        Error(msg, id, asCaller);
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
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_INT8: {
        int8 value = ArrayOf(c).getContentAsInteger8Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_UINT16: {
        uint16 value = ArrayOf(c).getContentAsUnsignedInteger16Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_INT16: {
        int16 value = ArrayOf(c).getContentAsInteger16Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_UINT32: {
        uint32 value = ArrayOf(c).getContentAsUnsignedInteger32Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_INT32: {
        int32 value = ArrayOf(c).getContentAsInteger32Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_UINT64: {
        uint64 value = ArrayOf(c).getContentAsUnsignedInt64Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_INT64: {
        int64 value = ArrayOf(c).getContentAsInteger64Scalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_SINGLE: {
        single value = ArrayOf(c).getContentAsSingleScalar();
        valueAsString = std::to_wstring(value);
    } break;
    case NLS_DOUBLE: {
        double value = ArrayOf(c).getContentAsDoubleScalar();
        valueAsString = std::to_wstring(value);
    } break;
    }
    std::wstring msg;
    if (valueAsString.empty()) {
        msg = defaultMessage;
    } else {
        msg = str(boost::wformat { fmt } % valueAsString);
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
    ArrayOfVector argOut = OperatorsGateway::gtBuiltin(_eval, 1, params);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeGreaterThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argC);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Second input to function 'mustBeGreaterThan' must be a scalar.");
        std::wstring id = _W("Nelson:validatorUsage:nonScalarSecondInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argC);
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Inputs to function 'mustBeGreaterThan' must be numeric or logical.");
        std::wstring id = _W("Nelson:validatorUsage:nonNumericOrLogicalInput");
        Error(msg, id, asCaller);
    }
    ArrayOfVector argIn(arg);
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        std::wstring id = _W("Nelson:validators:mustBeNumericOrLogical");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argC);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeGreaterThan' must be real.");
        std::wstring id = _W("Nelson:validatorUsage:nonRealInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    if (!mustBeGreaterThan(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be greater than %s."),
                _W("Value must be greater than compared value."));
        std::wstring id = _W("Nelson:validators:mustBeGreaterThan");
        Error(msg, id, asCaller);
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
    ArrayOfVector argOut = OperatorsGateway::ltBuiltin(_eval, 1, params);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeLessThan(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argC);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Second input to function 'mustBeLessThan' must be a scalar.");
        std::wstring id = _W("Nelson:validatorUsage:nonScalarSecondInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argC);
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Inputs to function 'mustBeLessThan' must be numeric or logical.");
        std::wstring id = _W("Nelson:validatorUsage:nonNumericOrLogicalInput");
        Error(msg, id, asCaller);
    }
    ArrayOfVector argIn(arg);
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        std::wstring id = _W("Nelson:validators:mustBeNumericOrLogical");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argC);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeLessThan' must be real.");
        std::wstring id = _W("Nelson:validatorUsage:nonRealInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    if (!mustBeLessThan(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be less than %s."),
                _W("Value must be less than compared value."));
        std::wstring id = _W("Nelson:validators:mustBeLessThan");
        Error(msg, id, asCaller);
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
    ArrayOfVector argOut = OperatorsGateway::geBuiltin(_eval, 1, params);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeGreaterThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argC);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg
            = _W("Second input to function 'mustBeGreaterThanOrEqual' must be a scalar.");
        std::wstring id = _W("Nelson:validatorUsage:nonScalarSecondInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argC);
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg
            = _W("Inputs to function 'mustBeGreaterThanOrEqual' must be numeric or logical.");
        std::wstring id = _W("Nelson:validatorUsage:nonNumericOrLogicalInput");
        Error(msg, id, asCaller);
    }
    ArrayOfVector argIn(arg);
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        std::wstring id = _W("Nelson:validators:mustBeNumericOrLogical");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argC);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeGreaterThanOrEqual' must be real.");
        std::wstring id = _W("Nelson:validatorUsage:nonRealInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    if (!mustBeGreaterThanOrEqual(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be greater than or equal to %s."),
                _W("Value must be greater than or equal to compared value."));
        std::wstring id = _W("Nelson:validators:mustBeGreaterThanOrEqual");
        Error(msg, id, asCaller);
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
    ArrayOfVector argOut = OperatorsGateway::leBuiltin(_eval, 1, params);
    argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
    return argOut[0].getContentAsLogicalScalar();
}
//=============================================================================
void
mustBeLessThanOrEqual(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argC(c);
    ArrayOfVector argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argC);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = _W("Second input to function 'mustBeLessThanOrEqual' must be a scalar.");
        std::wstring id = _W("Nelson:validatorUsage:nonScalarSecondInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argC);
    bool isLogical = (c.isLogical() || ClassName(c) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg
            = _W("Inputs to function 'mustBeLessThanOrEqual' must be numeric or logical.");
        std::wstring id = _W("Nelson:validatorUsage:nonNumericOrLogicalInput");
        Error(msg, id, asCaller);
    }
    ArrayOfVector argIn(arg);
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    if (!isNumeric && !isLogical) {
        std::wstring msg = _W("Value must be numeric or logical.");
        std::wstring id = _W("Nelson:validators:mustBeNumericOrLogical");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argC);
    bool isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Inputs to function 'mustBeLessThanOrEqual' must be real.");
        std::wstring id = _W("Nelson:validatorUsage:nonRealInput");
        Error(msg, id, asCaller);
    }
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    isReal = argOut[0].getContentAsLogicalScalar();
    if (!isReal) {
        std::wstring msg = _W("Value must be real.");
        std::wstring id = _W("Nelson:validators:mustBeReal");
        Error(msg, id, asCaller);
    }
    if (!mustBeLessThanOrEqual(arg, c)) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + createPrintableScalar(c, _W("Value must be less than or equal to %s."),
                _W("Value must be less than or equal to compared value."));
        std::wstring id = _W("Nelson:validators:mustBeLessThanOrEqual");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNumericOrLogical(const ArrayOf& arg, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    ArrayOfVector argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    bool isLogical = (arg.isLogical() || ClassName(arg) == "logical");
    bool isNumeric = argOut[0].getContentAsLogicalScalar();
    if (!isNumeric && !isLogical) {
        std::wstring msg
            = invalidPositionMessage(argPosition) + _W("Value must be numeric or logical.");
        std::wstring id = _W("Nelson:validators:mustBeNumericOrLogical");
        Error(msg, id, asCaller);
    }
}
//=============================================================================
void
mustBeNonzeroLengthText(const ArrayOf& arg, int argPosition, bool asCaller)
{
    bool isText = arg.isRowVectorCharacterArray() || arg.isStringArray() || IsCellOfString(arg);
    if (!isText) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be a character vector, string array or cell array of character "
                 "vectors.");
        std::wstring id = _W("Nelson:validators:mustBeNonzeroLengthText");
        Error(msg, id, asCaller);
    } else {
        ArrayOfVector argIn(arg);
        ArrayOfVector argOut = StringGateway::strlengthBuiltin(_eval, 1, argIn);
        argOut.push_back(ArrayOf::doubleConstructor(0));
        argOut = OperatorsGateway::gtBuiltin(_eval, 1, argOut);
        argOut.push_back(ArrayOf::characterArrayConstructor("all"));
        argOut = OperatorsGateway::allBuiltin(_eval, 1, argOut);
        if (!argOut[0].getContentAsLogicalScalar()) {
            std::wstring msg
                = invalidPositionMessage(argPosition) + _W("Value must be non zero length text.");
            std::wstring id = _W("Nelson:validators:mustBeNonzeroLengthText");
            Error(msg, id, asCaller);
        }
    }
}
//=============================================================================
void
mustBeMember(const ArrayOf& arg, const ArrayOf& c, int argPosition, bool asCaller)
{
    ArrayOfVector argIn(arg);
    argIn << c;
    ArrayOfVector argOut = OperatorsGateway::ismemberBuiltin(_eval, 1, argIn);
    Dimensions dims = argOut[0].getDimensions();
    Dimensions dimsV(1, dims.getElementCount());
    ArrayOf asVector = argOut[0];
    asVector.reshape(dimsV);
    argOut = OperatorsGateway::anyBuiltin(_eval, 1, asVector);
    if (!argOut[0].getContentAsLogicalScalar()) {
        std::wstring msg = invalidPositionMessage(argPosition)
            + _W("Value must be member of the compared value.");
        std::wstring id = _W("Nelson:validators:mustBeMember");
        Error(msg, id, asCaller);
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
            std::wstring msg = str(
                boost::wformat { _W("A combinaison of '%s' and '%s' options is not supported.") }
                % boundflag1 % boundflag2);
            std::wstring id = _W("Nelson:validatorUsage:ConflictingBoundaryOptions");
            Error(msg, id, asCaller);
        } else {
            std::wstring msg
                = _W("Value must 'inclusive', 'exclusive', 'exclude-lower' or 'exclude-upper'.");
            std::wstring id = _W("Nelson:validators:BoundInclusivity");
            Error(msg, id, asCaller);
        }
    }
    mustBeNumericOrLogical(value, 1, asCaller);
    mustBeReal(value, 1, asCaller);

    ArrayOfVector argIn(lower);
    ArrayOfVector argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    bool isnumeric = argOut[0].getContentAsLogicalScalar();
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    bool isreal = argOut[0].getContentAsLogicalScalar();
    argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
    bool isscalar = argOut[0].getContentAsLogicalScalar();
    bool isLogical = (argOut[0].isLogical() || ClassName(argOut[0]) == "logical");
    if (!((isnumeric && isreal) || isLogical) || !isscalar) {
        std::wstring id = _W("Nelson:validatorUsage:invalidLowerBound");
        std::wstring msg
            = _W("Second input to function 'mustBeInRange' must be a real or scalar value.");
        Error(msg, id, asCaller);
    }

    argIn = upper;
    argOut = TypeGateway::isnumericBuiltin(_eval, 1, argIn);
    isnumeric = argOut[0].getContentAsLogicalScalar();
    argOut = TypeGateway::isrealBuiltin(_eval, 1, argIn);
    isreal = argOut[0].getContentAsLogicalScalar();
    argOut = ElementaryFunctionsGateway::isscalarBuiltin(_eval, 1, argIn);
    isscalar = argOut[0].getContentAsLogicalScalar();
    isLogical = (argOut[0].isLogical() || ClassName(argOut[0]) == "logical");
    if (!((isnumeric && isreal) || isLogical) || !isscalar) {
        std::wstring id = _W("Nelson:validatorUsage:invalidLowerBound");
        std::wstring msg
            = _W("Third input to function 'mustBeInRange' must be a real or scalar value.");
        Error(msg, id, asCaller);
    }
    std::wstring id;
    if (includeLower) {
        if (includeUpper) {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = _W("Nelson:validators:LeftClosedRightClosed");
        } else {
            if (mustBeGreaterThanOrEqual(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = _W("Nelson:validators:LeftClosedRightOpen");
        }
    } else {
        if (includeUpper) {
            if (mustBeGreaterThan(value, lower) && mustBeLessThanOrEqual(value, upper)) {
                return;
            }
            id = _W("Nelson:validators:LeftOpenRightClosed");
        } else {
            if (mustBeGreaterThan(value, lower) && mustBeLessThan(value, upper)) {
                return;
            }
            id = _W("Nelson:validators:LeftOpenRightOpen");
        }
    }
    std::wstring msg = _W("Value must be in range.");
    Error(msg, id, asCaller);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
