//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsError_manager_exports.h"
#include "Exception.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSERROR_MANAGER_IMPEXP bool
isValidMExceptionIdentifier(std::wstring identifier);
//=============================================================================
inline ArrayOf
ExceptionToArrayOf(const Exception& e)
{
    Dimensions dimsRes(1, 1);
    stringVector fieldnames;
    fieldnames.reserve(5);
    fieldnames.push_back("identifier");
    fieldnames.push_back("message");
    fieldnames.push_back("cause");
    fieldnames.push_back("stack");
    fieldnames.push_back("Correction");
    ArrayOf* elementRes = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CLASS_ARRAY, dimsRes.getElementCount(), fieldnames, false));
    ArrayOf res = ArrayOf(NLS_CLASS_ARRAY, dimsRes, elementRes, false, fieldnames);
    res.setClassType("MException");

    std::vector<Exception> causeExceptions = e.getCause();
    ArrayOf causeCell;
    if (causeExceptions.empty()) {
        Dimensions dimsEmptyCell(0, 0);
        causeCell = ArrayOf::emptyCell(dimsEmptyCell);
    } else {
        Dimensions dims(1, causeExceptions.size());
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount());
        causeCell = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        for (indexType k = 0; k < (indexType)causeExceptions.size(); ++k) {
            elements[k] = ExceptionToArrayOf(causeExceptions[k]);
        }
    }
    ArrayOf stack;
    std::vector<PositionScript> trace = e.getTrace();
    stringVector traceFieldnames(3);
    traceFieldnames[0] = "file";
    traceFieldnames[1] = "name";
    traceFieldnames[2] = "line";
    Dimensions dims(trace.size(), 1);
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), traceFieldnames, false));
    stack = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, traceFieldnames);
    ArrayOfVector files(trace.size());
    ArrayOfVector names(trace.size());
    ArrayOfVector lines(trace.size());
    for (auto& k : trace) {
        files.push_back(ArrayOf::characterArrayConstructor(k.getFilename()));
        names.push_back(ArrayOf::characterArrayConstructor(k.getFunctionName()));
        lines.push_back(ArrayOf::doubleConstructor(k.getLine()));
    }
    if (!trace.empty()) {
        stack.setFieldAsList(traceFieldnames[0], files);
        stack.setFieldAsList(traceFieldnames[1], names);
        stack.setFieldAsList(traceFieldnames[2], lines);
    }

    Dimensions dimsEmptyCell(0, 0);
    ArrayOf correction = ArrayOf::emptyCell(dimsEmptyCell);

    ArrayOfVector identifierVector(ArrayOf::characterArrayConstructor(e.getIdentifier()));
    ArrayOfVector messageVector(ArrayOf::characterArrayConstructor(e.getMessage()));
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
inline Exception
ArrayOfToException(const ArrayOf& arg)
{
    Exception e;
    ArrayOf copyArg = arg;
    ArrayOf idArrayOf = copyArg.getField("identifier");
    ArrayOf msgArrayOf = copyArg.getField("message");
    ArrayOf causeArrayOf = copyArg.getField("cause");
    ArrayOf stackArrayOf = copyArg.getField("stack");
    ArrayOf correctionArrayOf = copyArg.getField("Correction");

    std::wstring identifier = idArrayOf.getContentAsWideString();
    std::wstring message = msgArrayOf.getContentAsWideString();

    e.setIdentifier(identifier);
    e.setMessage(message);

    std::vector<Exception> cause;
    auto* cell = (ArrayOf*)causeArrayOf.getDataPointer();
    if (cell == nullptr) {
        return e;
    }
    for (indexType k = 0; k < causeArrayOf.getElementCount(); ++k) {
        cause.emplace_back(ArrayOfToException(cell[k]));
    }
    e.setCause(cause);

    auto* stackElement = (ArrayOf*)stackArrayOf.getDataPointer();
    std::vector<PositionScript> trace;
    for (indexType k = 0; k < stackArrayOf.getElementCount(); k++) {
        ArrayOf fileAsArrayOf = stackElement[k].getField("file");
        ArrayOf nameAsArrayOf = stackElement[k].getField("name");
        ArrayOf lineAsArrayOf = stackElement[k].getField("line");
        trace.emplace_back(nameAsArrayOf.getContentAsWideString(),
            fileAsArrayOf.getContentAsWideString(), (int)lineAsArrayOf.getContentAsDoubleScalar());
    }
    e.setTrace(trace);
    return e;
}
//=============================================================================
}
//=============================================================================
