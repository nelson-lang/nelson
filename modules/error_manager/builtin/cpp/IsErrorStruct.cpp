//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "IsErrorStruct.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool IsErrorStruct(ArrayOf arg, Exception &e)
    {
        if (!arg.isStruct())
        {
            return false;
        }
        stringVector fs = arg.getFieldNames();
        if (fs.size() != 3)
        {
            return false;
        }
        if (fs[0] != "message")
        {
            return false;
        }
        if (fs[1] != "identifier")
        {
            return false;
        }
        if (fs[2] != "stack")
        {
            return false;
        }
        ArrayOf stack = arg.getField("stack");
        if (!stack.isStruct())
        {
            return false;
        }
        stringVector stackFieldNames = stack.getFieldNames();
        if (stackFieldNames.size() != 3)
        {
            return false;
        }
        if (stackFieldNames[0] != "file")
        {
            return false;
        }
        if (stackFieldNames[1] != "name")
        {
            return false;
        }
        if (stackFieldNames[2] != "line")
        {
            return false;
        }
        std::wstring message = L"";
        std::wstring identifier = L"";
        std::wstring filename = L"";
        std::wstring name = L"";
        int line = -1;
        ArrayOf msgArray = arg.getField("message");
        ArrayOf idArray = arg.getField("identifier");
        if (!msgArray.isSingleString())
        {
            return false;
        }
        if (!idArray.isSingleString())
        {
            return false;
        }
        if (!stack.isEmpty())
        {
            ArrayOf fileArray = stack.getField("file");
            ArrayOf nameArray = stack.getField("name");
            ArrayOf lineArray = stack.getField("line");
            if (!fileArray.isSingleString())
            {
                return false;
            }
            if (!nameArray.isSingleString())
            {
                return false;
            }
            if (!lineArray.isDoubleType() || !lineArray.isScalar())
            {
                return false;
            }
            message = msgArray.getContentAsWideString();
            identifier = idArray.getContentAsWideString();
            filename = fileArray.getContentAsWideString();
            name = nameArray.getContentAsWideString();
            if (!lineArray.isEmpty())
            {
                line = (int)lineArray.getContentAsDoubleScalar();
            }
        }
        e.setMessage(message);
        e.setIdentifier(identifier);
        e.setFileName(filename);
        e.setFunctionName(name);
        e.setLinePosition(line, -1);
        return true;
    }
    //=============================================================================
    bool IsErrorStruct(const ArrayOf arg)
    {
        Exception e(L"");
        return IsErrorStruct(arg, e);
    }
    //=============================================================================
}
//=============================================================================
