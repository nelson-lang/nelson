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
#include "IsErrorStruct.hpp"
#include "Exception.hpp"
#include "PositionScript.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsErrorStruct(const ArrayOf& arg, Exception& e)
{
    if (!arg.isStruct()) {
        return false;
    }
    stringVector fs = arg.getFieldNames();
    if (fs.size() != 3) {
        return false;
    }
    if (fs[0] != "message") {
        return false;
    }
    if (fs[1] != "identifier") {
        return false;
    }
    if (fs[2] != "stack") {
        return false;
    }
    ArrayOf copyArg = arg;
    ArrayOf stack = copyArg.getField("stack");
    if (!stack.isStruct()) {
        return false;
    }
    stringVector stackFieldNames = stack.getFieldNames();
    if (stackFieldNames.size() != 3) {
        return false;
    }
    if (stackFieldNames[0] != "file") {
        return false;
    }
    if (stackFieldNames[1] != "name") {
        return false;
    }
    if (stackFieldNames[2] != "line") {
        return false;
    }
    std::wstring message;
    std::wstring identifier;
    std::wstring filename;
    std::wstring functionName;
    int line = -1;
    ArrayOf msgArray = copyArg.getField("message");
    ArrayOf idArray = copyArg.getField("identifier");
    if (!msgArray.isRowVectorCharacterArray()) {
        return false;
    }
    if (!idArray.isRowVectorCharacterArray()) {
        return false;
    }
    message = msgArray.getContentAsWideString();
    identifier = idArray.getContentAsWideString();
    if (!stack.isEmpty()) {
        stringVector fieldnames = stack.getFieldNames();
        if (fieldnames.size() != 3) {
            return false;
        }
        if (fieldnames[0] != "file") {
            return false;
        }
        if (fieldnames[1] != "name") {
            return false;
        }
        if (fieldnames[2] != "line") {
            return false;
        }
        ArrayOfVector fileArray = stack.getFieldAsList("file");
        ArrayOfVector nameArray = stack.getFieldAsList("name");
        ArrayOfVector lineArray = stack.getFieldAsList("line");
        for (const ArrayOf& element : fileArray) {
            if (!element.isRowVectorCharacterArray()) {
                return false;
            }
        }
        for (const ArrayOf& element : nameArray) {
            if (!element.isRowVectorCharacterArray()) {
                return false;
            }
        }
        for (const ArrayOf& element : lineArray) {
            if (!element.isDoubleType() || !element.isScalar()) {
                return false;
            }
        }

        std::vector<PositionScript> trace;
        trace.reserve(fileArray.size());
        for (indexType k = 0; k < fileArray.size(); k++) {
            std::wstring file = fileArray[k].getContentAsWideString();
            std::wstring name = nameArray[k].getContentAsWideString();
            int line = (int)lineArray[k].getContentAsDoubleScalar();
            trace.emplace_back(name, file, line);
        }
        e.setTrace(trace);
        e.setMessage(message);
        e.setIdentifier(identifier);
    } else {
        e.setMessage(message);
        e.setIdentifier(identifier);
    }
    return true;
}
//=============================================================================
bool
IsErrorStruct(const ArrayOf& arg)
{
    Exception e;
    return IsErrorStruct(arg, e);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
