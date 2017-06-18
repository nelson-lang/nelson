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
#include "errorBuiltin.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::ErrorManagerGateway::errorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, bSuccess);
    if (!bSuccess)
    {
        if (argIn[0].isSingleString())
        {
            std::wstring msg = argIn[0].getContentAsWideString();
            if (msg.compare(L"") != 0)
            {
                if (eval->getCallerFunctionName().compare("EvaluateScript") == 0)
                {
                    Error(eval, msg);
                }
                else
                {
                    std::wstring currentFilename = L"";
                    size_t sz = eval->cstack.size();
                    int line = -1;
                    int position = -1;
                    if (sz > 2)
                    {
                        line = eval->cstack[sz - 2].tokid & 0x0000FFFF;
                        position = eval->cstack[sz - 2].tokid >> 16;
                        std::wstring callerName = eval->getCallerFunctionNameW();
                        std::wstring fileName = L"";
                        if (callerName == L"EvaluateScript")
                        {
                            fileName = eval->getCurrentEvaluateFilename();
                        }
                        else
                        {
                            fileName = callerName;
                        }
                        currentFilename = fileName;
                    }
                    throw Exception(msg, eval->getCallerFunctionNameW(), line, position, currentFilename);
                }
            }
        }
        else
        {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
