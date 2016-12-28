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
#include "persistentBuiltin.hpp"
#include "IsValidVariableName.hpp"
#include "Error.hpp"
#include "StringFormat.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::MemoryGateway::persistentBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    Context *context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base")
    {
        Error(eval, _W("A 'persistent' declaration is only allowed in a script file function."));
    }
    for (size_t k = 0; k < argIn.size(); k++)
    {
        if (!argIn[k].isSingleString())
        {
            Error(eval, StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(), k + 1));
        }
        std::string arg = argIn[k].getContentsAsCString();
        if (!IsValidVariableName(arg))
        {
            Error(eval, _W("Argument must contain a valid variable name."));
        }
        if (context->isLockedVariable(arg))
        {
            Error(eval, _W("variable is locked."));
        }
    }
    for (size_t k = 0; k < argIn.size(); k++)
    {
        std::string arg = argIn[k].getContentsAsCString();
        context->addPersistentVariable(arg);
    }
    return retval;
}
//=============================================================================
