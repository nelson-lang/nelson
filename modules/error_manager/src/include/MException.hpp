//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <memory>
#include <string>
#include <utility>
#include "nlsError_manager_exports.h"
#include "NelsonException.hpp"
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Types.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr const char* MEXCEPTION_CLASS_NAME = "MException";
//=============================================================================
inline stringVector
MExceptionPropertyNames()
{
    return { "identifier", "message", "cause", "stack", "Correction" };
}
//=============================================================================
inline stringVector
MExceptionMethodNames()
{
    return { "MException", "addCause", "addCorrection", "eq", "getReport", "isequal", "last", "ne",
        "rethrow", "throw", "throwAsCaller" };
}
//=============================================================================
inline bool
isMExceptionArray(const ArrayOf& arg)
{
    return arg.isClassType() && ClassName(arg) == MEXCEPTION_CLASS_NAME;
}
//=============================================================================
inline bool
isMExceptionCorrectionObject(const ArrayOf& value)
{
    if (!value.isClassType()) {
        return false;
    }
    const std::string className = ClassName(value);
    return className == "nelson.lang.correction.AppendArgumentsCorrection"
        || className == "nelson.lang.correction.ConvertToFunctionNotationCorrection"
        || className == "nelson.lang.correction.ReplaceIdentifierCorrection";
}
//=============================================================================
inline ArrayOf
NelsonExceptionCorrectionToArrayOf(const NelsonException& e)
{
    if (!e.hasCorrection()) {
        return ArrayOf::empty(Dimensions(0, 0));
    }
    return *e.correctionHandle();
}
//=============================================================================
inline void
SetNelsonExceptionCorrection(NelsonException& e, const ArrayOf& correction)
{
    if (correction.isEmpty()) {
        e.setCorrectionHandle(nullptr);
        return;
    }
    e.setCorrectionHandle(std::make_shared<ArrayOf>(correction));
}
//=============================================================================
NLSERROR_MANAGER_IMPEXP bool
isValidMExceptionIdentifier(std::wstring identifier);
//=============================================================================
inline ArrayOf
NelsonExceptionToArrayOf(const NelsonException& e)
{
    Dimensions dimsRes(1, 1);
    stringVector fieldnames = MExceptionPropertyNames();
    ArrayOf* elementRes = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CLASSDEF_ARRAY, dimsRes.elementCount(), fieldnames, false));
    ArrayOf res = ArrayOf(NLS_CLASSDEF_ARRAY, dimsRes, elementRes, false, fieldnames);
    res.setClassType(MEXCEPTION_CLASS_NAME);

    const std::vector<NelsonException>& causeExceptions = e.cause();
    ArrayOf causeCell;
    if (causeExceptions.empty()) {
        causeCell = ArrayOf::emptyCell(Dimensions(0, 0));
    } else {
        Dimensions dims(1, causeExceptions.size());
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount());
        causeCell = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (indexType k = 0; k < (indexType)causeExceptions.size(); ++k) {
            elements[k] = NelsonExceptionToArrayOf(causeExceptions[k]);
        }
    }
    const std::vector<PositionScript>& trace = e.trace();
    stringVector traceFieldnames = { "file", "name", "line" };
    Dimensions dims(trace.size(), 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), traceFieldnames, false));
    ArrayOf stack = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, traceFieldnames);
    ArrayOfVector files;
    ArrayOfVector names;
    ArrayOfVector lines;
    for (const auto& k : trace) {
        files.push_back(ArrayOf::characterArray(k.getFilename()));
        names.push_back(ArrayOf::characterArray(k.getFunctionName()));
        lines.push_back(ArrayOf::doubleScalar(k.getLine()));
    }
    if (!trace.empty()) {
        stack.setFieldAsList(traceFieldnames[0], files);
        stack.setFieldAsList(traceFieldnames[1], names);
        stack.setFieldAsList(traceFieldnames[2], lines);
    }

    ArrayOf correction = NelsonExceptionCorrectionToArrayOf(e);
    ArrayOfVector identifierVector(ArrayOf::characterArray(e.identifier()));
    ArrayOfVector messageVector(ArrayOf::characterArray(e.message()));
    ArrayOfVector causeVector(causeCell);
    ArrayOfVector stackVector(stack);
    ArrayOfVector correctionVector(correction);
    res.setFieldAsList(fieldnames[0], identifierVector);
    res.setFieldAsList(fieldnames[1], messageVector);
    res.setFieldAsList(fieldnames[2], causeVector);
    res.setFieldAsList(fieldnames[3], stackVector);
    res.setFieldAsList(fieldnames[4], correctionVector);
    return res;
}
//=============================================================================
inline NelsonException
ArrayOfToNelsonException(const ArrayOf& arg)
{
    if (!isMExceptionArray(arg)) {
        Error(L"Nelson:MException:invalidType", L"MException expected.");
    }
    NelsonException e;
    ArrayOf copyArg = arg;
    ArrayOf idArrayOf = copyArg.field("identifier");
    ArrayOf msgArrayOf = copyArg.field("message");
    ArrayOf causeArrayOf = copyArg.field("cause");
    ArrayOf stackArrayOf = copyArg.field("stack");
    ArrayOf correctionArrayOf = copyArg.field("Correction");

    e.setIdentifier(idArrayOf.asWideString());
    e.setMessage(msgArrayOf.asWideString());

    std::vector<NelsonException> cause;
    auto* cell = (ArrayOf*)causeArrayOf.dataPointer();
    if (cell != nullptr) {
        for (indexType k = 0; k < causeArrayOf.elementCount(); ++k) {
            cause.emplace_back(ArrayOfToNelsonException(cell[k]));
        }
    }
    e.setCause(std::move(cause));

    ArrayOfVector files = stackArrayOf.fieldAsList("file");
    ArrayOfVector names = stackArrayOf.fieldAsList("name");
    ArrayOfVector lines = stackArrayOf.fieldAsList("line");
    std::vector<PositionScript> trace;
    for (indexType k = 0; k < stackArrayOf.elementCount(); k++) {
        trace.emplace_back(
            names[k].asWideString(), files[k].asWideString(), (int)lines[k].asDoubleScalar());
    }
    e.setTrace(std::move(trace));
    SetNelsonExceptionCorrection(e, correctionArrayOf);
    return e;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
