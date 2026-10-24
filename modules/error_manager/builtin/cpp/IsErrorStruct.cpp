//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <limits>
#include "IsErrorStruct.hpp"
#include "PositionScript.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
normalizeStackLine(const ArrayOf& value);
//=============================================================================
bool
IsErrorStruct(const ArrayOf& arg, NelsonException& e)
{
    if (!arg.isStruct()) {
        return false;
    }
    if (arg.isEmpty()) {
        e = NelsonException();
        return true;
    }
    if (!arg.isScalar()) {
        return false;
    }
    ArrayOf copyArg = arg;
    std::wstring message;
    std::wstring identifier;

    if (copyArg.fieldIndex("message") >= 0) {
        ArrayOf msgArray = copyArg.field("message");
        if (!msgArray.isRowVectorCharacterArray() && !msgArray.isScalarStringArray()) {
            return false;
        }
        message = msgArray.asWideString();
    }

    if (copyArg.fieldIndex("identifier") >= 0) {
        ArrayOf idArray = copyArg.field("identifier");
        if (!idArray.isRowVectorCharacterArray() && !idArray.isScalarStringArray()) {
            return false;
        }
        identifier = idArray.asWideString();
    }

    if (copyArg.fieldIndex("stack") >= 0) {
        ArrayOf stack = copyArg.field("stack");
        if (!stack.isStruct()) {
            return false;
        }
        if (stack.fieldIndex("file") < 0) {
            return false;
        }
        if (stack.fieldIndex("name") < 0) {
            return false;
        }
        if (stack.fieldIndex("line") < 0) {
            return false;
        }
        if (!stack.isEmpty()) {
            ArrayOfVector fileArray = stack.fieldAsList("file");
            ArrayOfVector nameArray = stack.fieldAsList("name");
            ArrayOfVector lineArray = stack.fieldAsList("line");
            for (const ArrayOf& element : fileArray) {
                if (!element.isRowVectorCharacterArray() && !element.isScalarStringArray()) {
                    return false;
                }
            }
            for (const ArrayOf& element : nameArray) {
                if (!element.isRowVectorCharacterArray() && !element.isScalarStringArray()) {
                    return false;
                }
            }
            std::vector<PositionScript> trace;
            trace.reserve(fileArray.size());
            for (indexType k = 0; k < fileArray.size(); k++) {
                std::wstring file = fileArray[k].asWideString();
                std::wstring name = nameArray[k].asWideString();
                int line = normalizeStackLine(lineArray[k]);
                trace.emplace_back(name, file, line);
            }
            e.setTrace(trace);
        }
    }
    e.setMessage(message);
    e.setIdentifier(identifier);
    return true;
}
//=============================================================================
bool
IsErrorStruct(const ArrayOf& arg)
{
    NelsonException e;
    return IsErrorStruct(arg, e);
}
//=============================================================================
static int
normalizeStackLine(const ArrayOf& value)
{
    if (!value.isNumeric() || !value.isScalar()) {
        return 0;
    }
    double line = value.asDoubleScalar();
    if (!std::isfinite(line) || line > std::numeric_limits<int>::max()
        || line < std::numeric_limits<int>::min()) {
        return 0;
    }
    return static_cast<int>(std::trunc(line));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
