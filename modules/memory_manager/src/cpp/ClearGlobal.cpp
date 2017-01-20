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
#include "ClearGlobal.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool ClearGlobalVariable(Evaluator* eval, std::wstring variable)
    {
        return ClearGlobalVariable(eval, wstring_to_utf8(variable));
    }
    //=============================================================================
    bool ClearGlobalVariable(Evaluator* eval, std::string variable)
    {
        return eval->getContext()->getGlobalScope()->deleteVariable(variable);
    }
    //=============================================================================
    bool ClearAllGlobalVariables(Evaluator* eval)
    {
        bool bUnlocked = true;
        stringVector names = eval->getContext()->getGlobalScope()->getVariablesList(true);
        for (size_t k = 0; k < names.size(); k++)
        {
            if (!eval->getContext()->getGlobalScope()->deleteVariable(names[k]))
            {
                bUnlocked = false;
            }
        }
        return bUnlocked;
    }
    //=============================================================================
	bool ClearAllPersistentVariables(Evaluator* eval)
	{
		bool bUnlocked = true;
		stringVector names = eval->getContext()->getGlobalScope()->getVariablesList(true);
		for (size_t k = 0; k < names.size(); k++)
		{
			if (!eval->getContext()->getGlobalScope()->isVariablePersistent(names[k]))
			{
				if (!eval->getContext()->getGlobalScope()->deleteVariable(names[k]))
				{
					bUnlocked = false;
				}
			}
		}
		return bUnlocked;
	}
	//=============================================================================
}
//=============================================================================
