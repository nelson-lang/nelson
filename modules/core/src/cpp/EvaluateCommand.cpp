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
#include <string>
#include "EvaluateCommand.hpp"
#include "characters_encoding.hpp"
#include "ParserInterface.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "AstManager.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	bool EvaluateCommand(Evaluator *eval, std::wstring command, bool bCatch)
	{
		return EvaluateCommand(eval, wstring_to_utf8(command), bCatch);
	}
	//=============================================================================
	bool EvaluateCommand(Evaluator *eval, std::string command, bool bCatch)
	{
		return eval->evaluateString(command, bCatch);
	}
	//=============================================================================
#define TEMP_VAR_NAME L"__eval_tmp_"
	//=============================================================================
	static std::wstring prepareVariablesReturned(int nLhs, std::wstring command)
	{
		std::wstring variables;
		if (nLhs > 1)
		{
			variables.append(L"[");
		}
		for (int v = 0; v < nLhs - 1; v++)
		{
			variables.append(std::wstring(TEMP_VAR_NAME) + std::to_wstring(v) + L", ");
		}
		variables.append(std::wstring(TEMP_VAR_NAME) + std::to_wstring(nLhs - 1));
		if (nLhs > 1)
		{
			variables.append(L"] = ");
			variables.append(command);
		}
		else
		{
			variables.append(L" = ");
			variables.append(command);
		}
		variables.append(L";");
		return variables;
	}
	//=============================================================================
	static ArrayOfVector retrieveVariablesReturned(Evaluator *eval, int nLhs)
	{
		ArrayOfVector variables;
		for (int v = 0; v < nLhs; v++)
		{
			std::wstring varname = std::wstring(TEMP_VAR_NAME) + std::to_wstring(v);
			ArrayOf var;
			if (!eval->getContext()->lookupVariableLocally(wstring_to_utf8(varname), var))
			{
				var = ArrayOf::emptyConstructor();
			}
			else
			{
				variables.push_back(var);
			}
			eval->getContext()->deleteVariable(wstring_to_utf8(varname));
		}
		return variables;
	}
	//=============================================================================
	ArrayOfVector EvaluateCommand(Evaluator *eval, int nLhs, std::wstring command, std::wstring catchCommand)
	{
		ArrayOfVector retval;
		if (catchCommand.empty())
		{
			if (nLhs == 0)
			{
				eval->getContext()->bypassScope(0);
				eval->evaluateString(command);
				eval->getContext()->restoreBypassedScopes();
			}
			else
			{
				std::wstring preparedCommand = prepareVariablesReturned(nLhs, command);
				eval->getContext()->bypassScope(0);
				eval->evaluateString(preparedCommand);
				retval = retrieveVariablesReturned(eval, nLhs);
				eval->getContext()->restoreBypassedScopes();
				if (retval.size() != nLhs)
				{
					Error(eval, _W("Invalid use of statement list."));
				}
			}
		}
		else
		{
			if (nLhs == 0)
			{
				bool autostop = eval->AutoStop();
				eval->AutoStop(false);
				try
				{
					eval->getContext()->bypassScope(0);
					eval->evaluateString(command, true);
					eval->getContext()->restoreBypassedScopes();
				}
				catch (Exception)
				{
					eval->getContext()->restoreBypassedScopes();
					eval->evaluateString(catchCommand, false);
				}
				eval->AutoStop(true);
			}
			else
			{
				std::wstring preparedCommand = prepareVariablesReturned(nLhs, command);
				std::wstring preparedCatchCommand = prepareVariablesReturned(nLhs, catchCommand);
				bool autostop = eval->AutoStop();
				eval->AutoStop(false);
				try
				{
					eval->getContext()->bypassScope(0);
					eval->evaluateString(preparedCommand, true);
					retval = retrieveVariablesReturned(eval, nLhs);
					eval->getContext()->restoreBypassedScopes();
				}
				catch (Exception)
				{
					eval->getContext()->restoreBypassedScopes();
					eval->evaluateString(preparedCatchCommand, false);
					retval = retrieveVariablesReturned(eval, nLhs);
				}
				eval->AutoStop(true);
				if (retval.size() != nLhs)
				{
					Error(eval, _W("Invalid use of statement list."));
				}
			}
		}
		return retval;
	}
	//=============================================================================
}
//=============================================================================
