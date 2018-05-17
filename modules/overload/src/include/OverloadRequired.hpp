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
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	typedef enum { FUNCTION = 0, UNARY, BINARY, TRINARY } OVERLOAD_TYPE;
	//=============================================================================
	static inline void OverloadRequired(Evaluator *eval, const ArrayOfVector& argIn, OVERLOAD_TYPE otype, const std::string &functionName = "")
	{
		std::string _functionName = eval->getCurrentFunctionName();
		if (!functionName.empty())
		{
			_functionName = functionName;
		}
		std::string OverloadName("");
		switch (otype)
		{
		case Nelson::BINARY:
			OverloadName = ClassName(argIn[0]) + "_" + _functionName + "_" + ClassName(argIn[1]);
			break;
		case Nelson::TRINARY:
			OverloadName = _functionName + "_" + ClassName(argIn[0]) + "_" + ClassName(argIn[1]) + "_" + ClassName(argIn[2]);
			break;
		case Nelson::UNARY:
		case Nelson::FUNCTION:
			OverloadName = ClassName(argIn[0]) + "_" + _functionName;
			break;
		default:
			Error(eval, _W("Wrong OVERLOAD_TYPE."));
			break;
		}
		Error(eval, _("function") + " " + OverloadName + " " + _("undefined."));
	}
	//=============================================================================
}
//=============================================================================
