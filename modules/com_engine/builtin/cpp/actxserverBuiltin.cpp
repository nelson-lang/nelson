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
#include "actxserverBuiltin.hpp"
#include "ActiveXServer.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::actxserverBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring progid = L"";
    std::wstring machine = L"";
    switch (argIn.size()) {
    case 1: {
        progid = argIn[0].getContentAsWideString();
    } break;
    case 3: {
        std::wstring type = argIn[2].getContentAsWideString();
        if (!(type == L"machine")) {
            Error(_W("'machine' value expected."));
        }
        progid = argIn[0].getContentAsWideString();
    } break;
    case 2:
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    ComHandleObject* comhandle = ActiveXServer(progid, machine);
    retval.push_back(ArrayOf::handleConstructor(comhandle));
#else
    Error(_W("Not implemented on this platform."));
#endif
    return retval;
}
//=============================================================================
