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
        ArrayOf fileArray = stack.getField("file");
        ArrayOf nameArray = stack.getField("name");
        ArrayOf lineArray = stack.getField("line");
        if (!fileArray.isRowVectorCharacterArray()) {
            return false;
        }
        if (!nameArray.isRowVectorCharacterArray()) {
            return false;
        }
        if (!lineArray.isDoubleType() || !lineArray.isScalar()) {
            return false;
        }
        filename = fileArray.getContentAsWideString();
        functionName = nameArray.getContentAsWideString();
        if (!lineArray.isEmpty()) {
            line = static_cast<int>(lineArray.getContentAsDoubleScalar());
        }
        PositionScript position(functionName, filename, line);
        Exception newException(message, position, identifier);
        e = newException;
    } else {
        Exception newException(message, identifier);
        e = newException;
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
