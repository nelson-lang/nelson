//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ErrorFunction.hpp"
#include <utility>
#include <cmath>
#include <vector>
#include "DebugStack.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Evaluator.hpp"
#include "IsErrorStruct.hpp"
#include "MException.hpp"
#include "ClassName.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOfVector
copyArgumentsFrom(const ArrayOfVector& argIn, size_t first);
static bool
hasThrowableContent(const NelsonException& exception);
static NelsonException
parseErrorStruct(const ArrayOf& arg, bool& useCurrentStack);
static std::wstring
asErrorMessageText(const ArrayOf& arg);
static bool
isMessageObject(const ArrayOf& arg);
static NelsonException
parseMessageObject(const ArrayOf& arg);
//=============================================================================
ErrorParseResult
ParseErrorFunctionArguments(const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }

    ErrorParseResult result;
    size_t first = 0;
    ArrayOf correction = ArrayOf::empty(Dimensions(0, 0));
    if (isMExceptionCorrectionObject(argIn[0])) {
        correction = argIn[0];
        first = 1;
        if (first == argIn.size()) {
            Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
        }
    }

    size_t remaining = argIn.size() - first;
    if (remaining == 1 && isMessageObject(argIn[first])) {
        result.exception = parseMessageObject(argIn[first]);
    } else if (remaining == 1 && argIn[first].isStruct()) {
        result.exception = parseErrorStruct(argIn[first], result.useCurrentStack);
    } else if (remaining == 1) {
        result.exception = NelsonException(asErrorMessageText(argIn[first]));
    } else {
        std::wstring firstText = asErrorMessageText(argIn[first]);
        result.needsFormatting = true;
        if (isValidMExceptionIdentifier(firstText)) {
            result.exception.setIdentifier(firstText);
            result.formatArguments = copyArgumentsFrom(argIn, first + 1);
            // When the inline message is exactly the English catalog template of this
            // identifier, use the localized template. Any other message (pre-formatted,
            // unrelated, or an identifier absent from the catalog) is left unchanged.
            if (!result.formatArguments.empty()
                && (result.formatArguments[0].isRowVectorCharacterArray()
                    || (result.formatArguments[0].isStringArray()
                        && result.formatArguments[0].isScalar()))) {
                std::wstring englishTemplate = getEnglishMessageTemplateById(firstText);
                if (!englishTemplate.empty()
                    && result.formatArguments[0].asWideString() == englishTemplate) {
                    result.formatArguments[0]
                        = ArrayOf::characterArray(getMessageTemplateById(firstText));
                }
            }
        } else {
            result.formatArguments = copyArgumentsFrom(argIn, first);
        }
    }

    if (!correction.isEmpty()) {
        SetNelsonExceptionCorrection(result.exception, correction);
    }
    return result;
}
//=============================================================================
void
ThrowParsedError(Evaluator* eval, NelsonException exception, bool useCurrentStack)
{
    if (!hasThrowableContent(exception)) {
        return;
    }
    if (useCurrentStack) {
        stackTrace trace;
        DebugStack(eval->callstack, 0, trace);
        exception.setTrace(std::move(trace));
    }
    throw exception;
}
//=============================================================================
static ArrayOfVector
copyArgumentsFrom(const ArrayOfVector& argIn, size_t first)
{
    ArrayOfVector result;
    result.reserve(argIn.size() - first);
    for (size_t k = first; k < argIn.size(); ++k) {
        result << argIn[k];
    }
    return result;
}
//=============================================================================
static bool
hasThrowableContent(const NelsonException& exception)
{
    return !exception.message().empty() || !exception.identifier().empty()
        || !exception.trace().empty() || exception.hasCorrection();
}
//=============================================================================
static NelsonException
parseErrorStruct(const ArrayOf& arg, bool& useCurrentStack)
{
    if (!arg.isStruct()) {
        Error(L"Nelson:error:invalidMessage",
            L"The message must be specified as either a character vector, string scalar, or a "
            L"message structure.");
    }
    if (!arg.isEmpty() && !arg.isScalar()) {
        Error(L"Nelson:error:structNotScalar", L"Input structure must be scalar.");
    }
    NelsonException exception;
    if (!IsErrorStruct(arg, exception)) {
        Error(L"Nelson:error:invalidErrorStruct",
            L"Wrong value for input argument: valid error structure expected.");
    }
    useCurrentStack = !arg.isEmpty() && arg.fieldIndex("stack") < 0;
    return exception;
}
//=============================================================================
static std::wstring
asErrorMessageText(const ArrayOf& arg)
{
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        return arg.asWideString();
    }
    Error(L"Nelson:error:invalidMessage",
        L"The message must be specified as either a character vector, string scalar, or a message "
        L"structure.");
    return L"";
}
//=============================================================================
static bool
isMessageObject(const ArrayOf& arg)
{
    return arg.isClassType() && arg.isScalar() && ClassName(arg) == "message";
}
//=============================================================================
static std::wstring
messageArgumentToString(const ArrayOf& argument)
{
    if (argument.isRowVectorCharacterArray() || (argument.isStringArray() && argument.isScalar())) {
        return argument.asWideString();
    }
    if (argument.isScalar() && (argument.isNumeric() || argument.isLogical())) {
        double value = argument.asDoubleScalar();
        if (value == std::floor(value) && std::isfinite(value)) {
            return std::to_wstring(static_cast<long long>(value));
        }
        return std::to_wstring(value);
    }
    return L"";
}
//=============================================================================
static NelsonException
parseMessageObject(const ArrayOf& arg)
{
    ArrayOf messageObject = arg;
    std::wstring identifier = messageObject.field("Identifier").asWideString();
    std::wstring messageTemplate = getMessageTemplateById(identifier);
    if (messageTemplate.empty()) {
        messageTemplate = identifier;
    }
    std::vector<std::wstring> arguments;
    ArrayOf argumentsCell = messageObject.field("Arguments");
    if (argumentsCell.isCell()) {
        auto* elements = (ArrayOf*)argumentsCell.dataPointer();
        if (elements != nullptr) {
            indexType count = argumentsCell.elementCount();
            arguments.reserve(count);
            for (indexType k = 0; k < count; ++k) {
                arguments.push_back(messageArgumentToString(elements[k]));
            }
        }
    }
    NelsonException exception;
    exception.setIdentifier(identifier);
    exception.setMessage(formatI18nMessageV(messageTemplate, arguments));
    return exception;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
