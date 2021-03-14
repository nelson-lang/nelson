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
#pragma once
//=============================================================================
#include "nlsInterpreter_exports.h"
#include <string>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
class PositionScript
{
    //=============================================================================
private:
    std::wstring filename = L"";
    std::wstring functionname = L"";
    int line = -1;
    //=============================================================================
public:
    //=============================================================================
    PositionScript(std::wstring functionname = L"", std::wstring filename = L"", int line = -1)
        : filename(std::move(filename)), functionname(std::move(functionname)), line(line)
    {}
    //=============================================================================
    PositionScript(const PositionScript& copy)
    {
        this->filename = copy.filename;
        this->functionname = copy.functionname;
        this->line = copy.line;
    }
    //=============================================================================
    void
    operator=(const PositionScript& copy)
    {
        if (this == &copy) {
            return;
        }
        this->filename = copy.filename;
        this->functionname = copy.functionname;
        this->line = copy.line;
    }
    //=============================================================================
    ~PositionScript()
    {
        this->filename.clear();
        this->functionname.clear();
        this->line = -1;
    }
    //=============================================================================
    std::wstring
    getFilename() const
    {
        return this->filename;
    }
    //=============================================================================
    void
    setFilename(const std::wstring& filename)
    {
        this->filename = filename;
    }
    //=============================================================================
    int
    getLine() const
    {
        return this->line;
    }
    //=============================================================================
    void
    setFunctionName(const std::wstring& functionname)
    {
        this->functionname = functionname;
    }
    //=============================================================================
    std::wstring
    getFunctionName() const
    {
        return this->functionname;
    }
    //=============================================================================
    bool
    isEmpty() const
    {
        return (this->functionname.empty() && this->filename.empty() && this->line == -1);
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
