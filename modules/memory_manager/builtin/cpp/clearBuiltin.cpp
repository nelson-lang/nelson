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
#include <boost/filesystem.hpp>
#include "clearBuiltin.hpp"
#include "Error.hpp"
#include "Clear.hpp"
#include "ClearGlobal.hpp"
#include "ClearFunction.hpp"
#include "characters_encoding.hpp"
#include "IsValidVariableName.hpp"
#include "StringFormat.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// clear keyword
// clear varname
// clear global varname
// clear varname1 varname2 ... varnameN
// clear function
ArrayOfVector Nelson::MemoryGateway::clearBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0)
    {
        ClearAllVariables(eval);
    }
    else
    {
        for (size_t k = 0; k < argIn.size(); k++)
        {
            if (!argIn[k].isSingleString())
            {
                Error(eval, StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(), k + 1));
            }
        }
        if (argIn.size() == 1)
        {
            std::wstring arg1 = argIn[0].getContentsAsWideString();
            if (arg1.compare(L"global") == 0)
            {
                ClearAllGlobalVariables(eval);
            }
            else if (arg1.compare(L"all") == 0)
            {
                ClearAllVariables(eval);
                ClearAllGlobalVariables(eval);
                ClearMacroCache(eval);
            }
            else if (arg1.compare(L"variables") == 0)
            {
                ClearAllVariables(eval);
            }
            else if (arg1.compare(L"functions") == 0)
            {
                ClearMacroCache(eval);
                ClearAllPersistentVariables(eval);
            }
            else
            {
                if (!IsValidVariableName(arg1))
                {
                    Error(eval, _W("A valid variable name expected."));
                }
                Context *ctxt = eval->getContext();
                if (ctxt->isLockedVariable(wstring_to_utf8(arg1)))
                {
                    Error(eval, _W("variable is locked:") + arg1);
                }
                ClearVariable(eval, arg1);
            }
        }
        else if (argIn.size() == 2)
        {
            // clear global varname
            // clear varname1 varname2
            std::wstring arg1 = argIn[0].getContentsAsWideString();
            std::wstring arg2 = argIn[1].getContentsAsWideString();
            Context *ctxt = eval->getContext();
            if (arg1 == L"global")
            {
                if (ctxt->getGlobalScope()->isLockedVariable(wstring_to_utf8(arg2)))
                {
                    Error(eval, _W("variable is locked:") + arg2);
                }
                ClearGlobalVariable(eval, arg2);
            }
            else
            {
                for (size_t k = 0; k < argIn.size(); k++)
                {
                    std::wstring arg = argIn[k].getContentsAsWideString();
                    if (!IsValidVariableName(arg))
                    {
                        Error(eval, _W("A valid variable name expected."));
                    }
                    if (ctxt->isLockedVariable(wstring_to_utf8(arg)))
                    {
                        Error(eval, _W("variable is locked:") + arg);
                    }
                    ClearVariable(eval, arg);
                }
            }
        }
        else
        {
            // clear varname1 varname2 ... varnameN
            Context *ctxt = eval->getContext();
            for (size_t k = 0; k < argIn.size(); k++)
            {
                std::wstring arg = argIn[k].getContentsAsWideString();
                if (!IsValidVariableName(arg))
                {
                    Error(eval, _W("A valid variable name expected."));
                }
                if (ctxt->isLockedVariable(wstring_to_utf8(arg)))
                {
                    Error(eval, _W("variable is locked:") + arg);
                }
                ClearVariable(eval, arg);
            }
        }
    }
    return retval;
}
//=============================================================================
