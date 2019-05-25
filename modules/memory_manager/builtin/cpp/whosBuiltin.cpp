//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "whosBuiltin.hpp"
#include "Error.hpp"
#include "Whos.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::whosBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring filename;
    Nelson::SCOPE_LEVEL scope = Nelson::SCOPE_LEVEL::LOCAL_SCOPE;
    stringVector names;
    bool onlyGlobal = false;
    switch (argIn.size()) {
    case 0: {
        filename.clear();
        names.clear();
    } break;
    case 1: {
        std::string param = argIn[0].getContentAsCString();
        if (param == "global") {
            onlyGlobal = true;
        } else if (param == "-file") {
            Error(_W("filename expected after '-file'."));
        } else {
            names.push_back(param);
        }
    } break;
    default: {
        std::string param1 = argIn[0].getContentAsCString();
        indexType start = 1;
        if (param1 == "global") {
            onlyGlobal = true;
        } else if (param1 == "-file") {
            filename = argIn[1].getContentAsWideString();
            start++;
        } else {
            names.push_back(param1);
        }
        for (indexType k = start; k < argIn.size(); ++k) {
            std::string param = argIn[k].getContentAsCString();
            if (param == "-file") {
                Error(_W("-file must be the first argument."));
            }
            names.push_back(param);
        }
    } break;
    }
    ArrayOf res = Whos(eval, filename, onlyGlobal, names, nLhs == 1);
    if (nLhs == 1) {
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
