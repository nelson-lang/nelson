//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "nlsInterpreter_exports.h"
#include <boost/container/vector.hpp>
//=============================================================================
namespace Nelson {
class ErrorInfo
{
private:
    std::wstring filename;
    std::wstring functionname;
    int lineposition;
    int columposition;

public:
    ErrorInfo()
    {
        this->filename = L"";
        this->functionname = L"";
        this->lineposition = -1;
        this->columposition = -1;
    };
    ~ErrorInfo()
    {
        this->filename = L"";
        this->functionname = L"";
        this->lineposition = -1;
        this->columposition = -1;
    };
    void
    set(std::wstring _filename, std::wstring _functionname, int _lineposition, int _columposition)
    {
        this->filename = _filename;
        this->functionname = _functionname;
        this->lineposition = _lineposition;
        this->columposition = _columposition;
    }
    void
    get(std::wstring& _filename, std::wstring& _functionname, int& _lineposition,
        int& _columposition)
    {
        _filename = this->filename;
        _functionname = this->functionname;
        _lineposition = this->lineposition;
        _columposition = this->columposition;
    }
};

NLSINTERPRETER_IMPEXP boost::container::vector<ErrorInfo>
StackError(Evaluator* eval);
}; // namespace Nelson
//=============================================================================
