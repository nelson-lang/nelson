//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "MExceptionBuiltin.hpp"
#include "MException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "ClassName.hpp"
#include "StringPrintf.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
isScalarMException(const ArrayOf& value)
{
    return isMExceptionArray(value) && value.isScalar();
}
//=============================================================================
void
checkScalarMException(const ArrayOf& value)
{
    if (!isMExceptionArray(value)) {
        Error(L"Nelson:MException:invalidType", L"MException expected.");
    }
    if (!value.isScalar()) {
        Error(L"Nelson:MException:notScalar", L"MException scalar expected.");
    }
}
//=============================================================================
bool
isCorrectionObject(const ArrayOf& value)
{
    return isMExceptionCorrectionObject(value);
}
//=============================================================================
ArrayOf
field(const ArrayOf& value, const std::string& fieldname)
{
    ArrayOf copy = value;
    return copy.field(fieldname);
}
//=============================================================================
bool
sameStack(const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (!lhs.dimensions().equals(rhs.dimensions())) {
        return false;
    }
    auto* left = static_cast<ArrayOf*>(const_cast<void*>(lhs.dataPointer()));
    auto* right = static_cast<ArrayOf*>(const_cast<void*>(rhs.dataPointer()));
    for (indexType k = 0; k < lhs.elementCount(); ++k) {
        if (left[k].field("file").asWideString() != right[k].field("file").asWideString()
            || left[k].field("name").asWideString() != right[k].field("name").asWideString()
            || left[k].field("line").asDoubleScalar() != right[k].field("line").asDoubleScalar()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
sameMExceptionScalar(const ArrayOf& lhs, const ArrayOf& rhs)
{
    if (!isScalarMException(lhs) || !isScalarMException(rhs)) {
        return false;
    }
    if (field(lhs, "identifier").asWideString() != field(rhs, "identifier").asWideString()
        || field(lhs, "message").asWideString() != field(rhs, "message").asWideString()) {
        return false;
    }
    ArrayOf leftCause = field(lhs, "cause");
    ArrayOf rightCause = field(rhs, "cause");
    if (!leftCause.dimensions().equals(rightCause.dimensions())) {
        return false;
    }
    auto* leftCauseElements = static_cast<ArrayOf*>(const_cast<void*>(leftCause.dataPointer()));
    auto* rightCauseElements = static_cast<ArrayOf*>(const_cast<void*>(rightCause.dataPointer()));
    for (indexType k = 0; k < leftCause.elementCount(); ++k) {
        if (!sameMExceptionScalar(leftCauseElements[k], rightCauseElements[k])) {
            return false;
        }
    }
    if (!sameStack(field(lhs, "stack"), field(rhs, "stack"))) {
        return false;
    }
    ArrayOf leftCorrection = field(lhs, "Correction");
    ArrayOf rightCorrection = field(rhs, "Correction");
    if (leftCorrection.dataClass() != rightCorrection.dataClass()
        || !leftCorrection.dimensions().equals(rightCorrection.dimensions())
        || ClassName(leftCorrection) != ClassName(rightCorrection)) {
        return false;
    }
    return true;
}
//=============================================================================
std::wstring
formatMExceptionReport(const NelsonException& exception, bool extended)
{
    std::wstring report = extended ? exception.formattedErrorMessage() : exception.message();
    const std::vector<NelsonException>& causes = exception.cause();
    if (!extended || causes.empty()) {
        return report;
    }
    report += L"\n" + _W("Caused by:") + L"\n";
    for (const NelsonException& cause : causes) {
        report += L"    " + cause.message() + L"\n";
    }
    return report;
}
//=============================================================================
ArrayOf
formatMessage(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector sprintfArgs;
    sprintfArgs.reserve(argIn.size() - 1);
    for (size_t k = 1; k < argIn.size(); ++k) {
        sprintfArgs << argIn[k];
    }
    std::wstring result;
    std::wstring errorMessage;
    if (!StringPrintf(result, errorMessage, eval, sprintfArgs)) {
        Error(L"Nelson:MException:formatFailed", errorMessage);
    }
    return ArrayOf::characterArray(result);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MExceptionBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    std::wstring identifier = argIn[0].asWideString();
    if (!isValidMExceptionIdentifier(identifier)) {
        Error(L"Nelson:MException:invalidIdentifier",
            L"First input argument must be a valid message identifier.");
    }
    ArrayOf message = formatMessage(eval, argIn);
    retval << NelsonExceptionToArrayOf(NelsonException(message.asWideString(), identifier));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_lastBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        std::wstring command = argIn[0].asWideString();
        if (command != L"reset") {
            Error("Nelson:MException:invalidValue", "%s expected.", "'reset' value");
        }
        eval->resetLastUncaughtException();
        return {};
    }
    return NelsonExceptionToArrayOf(eval->getLastUncaughtException());
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_addCauseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    checkScalarMException(argIn[0]);
    checkScalarMException(argIn[1]);
    ArrayOf result(argIn[0]);
    ArrayOf cause = field(result, "cause");
    auto* oldElements = static_cast<ArrayOf*>(const_cast<void*>(cause.dataPointer()));
    Dimensions dims(cause.elementCount() + 1, 1);
    auto* elements
        = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount()));
    for (indexType k = 0; k < cause.elementCount(); ++k) {
        elements[k] = oldElements[k];
    }
    elements[cause.elementCount()] = argIn[1];
    ArrayOf newCause(NLS_CELL_ARRAY, dims, elements);
    ArrayOfVector value(newCause);
    result.setFieldAsList("cause", value);
    return result;
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_addCorrectionBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    checkScalarMException(argIn[0]);
    if (!isCorrectionObject(argIn[1])) {
        Error(L"Nelson:MException:invalidCorrection", L"Correction object expected.");
    }
    ArrayOf result(argIn[0]);
    ArrayOfVector value(argIn[1]);
    result.setFieldAsList("Correction", value);
    return result;
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_getReportBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    checkScalarMException(argIn[0]);
    bool extended = true;
    if (argIn.size() >= 2) {
        std::wstring type = argIn[1].asWideString();
        if (type == L"basic") {
            extended = false;
        } else if (type != L"extended") {
            Error(
                L"Nelson:MException:invalidValue", L"%s expected.", L"'basic' or 'extended' value");
        }
    }
    if (argIn.size() == 4) {
        std::wstring option = argIn[2].asWideString();
        std::wstring value = argIn[3].asWideString();
        if (option != L"hyperlinks" || (value != L"default" && value != L"on" && value != L"off")) {
            Error(L"Nelson:MException:wrongInputValue", L"Wrong value for input argument.");
        }
    } else if (argIn.size() == 3) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    NelsonException NelsonException = ArrayOfToNelsonException(argIn[0]);
    return ArrayOf::characterArray(formatMExceptionReport(NelsonException, extended));
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_eqBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    return ArrayOf::logicalScalar(sameMExceptionScalar(argIn[0], argIn[1]));
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_neBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    return ArrayOf::logicalScalar(!sameMExceptionScalar(argIn[0], argIn[1]));
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    for (const ArrayOf& arg : argIn) {
        if (!sameMExceptionScalar(argIn[0], arg)) {
            return ArrayOf::logicalScalar(false);
        }
    }
    return ArrayOf::logicalScalar(true);
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_methodsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if ((argIn[0].isRowVectorCharacterArray() && argIn[0].asUtf8String() == MEXCEPTION_CLASS_NAME)
        || isMExceptionArray(argIn[0])) {
        return ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(MExceptionMethodNames());
    }
    Error(L"Nelson:MException:invalidType", L"MException expected.");
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_propertiesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if ((argIn[0].isRowVectorCharacterArray() && argIn[0].asUtf8String() == MEXCEPTION_CLASS_NAME)
        || isMExceptionArray(argIn[0])) {
        return ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(MExceptionPropertyNames());
    }
    Error(L"Nelson:MException:invalidType", L"MException expected.");
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::MException_subsasgnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    checkScalarMException(argIn[0]);
    Error(L"Nelson:MException:readOnlyProperty", L"Cannot set read-only property of MException.");
    return {};
}
//=============================================================================
