//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#include "profileBuiltin.hpp"
#include "Error.hpp"
#include "Profiler.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ProfilerGateway::profileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::string arg1AsString = param1.getContentAsCString();
    bool validOption = false;
    if (arg1AsString == "on") {
        Profiler::getInstance()->on();
        validOption = true;
    }
    if (arg1AsString == "off") {
        Profiler::getInstance()->off();
        validOption = true;
    }
    if (arg1AsString == "resume") {
        Profiler::getInstance()->resume();
        validOption = true;
    }
    if (arg1AsString == "info") {
        std::unordered_map<std::string, std::vector<uint64>> info
            = Profiler::getInstance()->info();
        validOption = true;
    }
    if (!validOption) {
        Error(_W("option not managed."));
    }
    return retval;
}
//=============================================================================
