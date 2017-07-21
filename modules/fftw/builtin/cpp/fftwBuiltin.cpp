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
#include "fftwBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "FftHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::FftwGateway::fftwBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
	// str = fftw('dwisdom')
	// str = fftw('swisdom')
	// method = fftw('planner')
	// previous = fftw('planner', method)
	// str = fftw('dwisdom', str)
	// previous = fftw('swisdom', str)
	ArrayOfVector retval;
	if (nLhs > 1)
	{
		Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
	}
	if (argIn.size() == 0 || argIn.size() > 2)
	{
		Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
	}
	if (argIn.size() == 1)
	{
		ArrayOf param1 = argIn[0];
		std::wstring infoDesired = param1.getContentAsWideString();
		ArrayOf res;
		if (infoDesired == L"dwisdom")
		{
			res = ArrayOf::stringConstructor(getDoubleWisdomInformation());
		} 
		else if (infoDesired == L"swisdom")
		{
			res = ArrayOf::stringConstructor(getSingleWisdomInformation());
		}
		else if (infoDesired == L"planner")
		{
			res = ArrayOf::stringConstructor(getPlannerInformation());
		}
		else
		{
			Error(eval, ERROR_WRONG_ARGUMENT_1_VALUE);
		}
		retval.push_back(res);
	}
	else
	{
		ArrayOf param1 = argIn[0];
		std::wstring fieldname = param1.getContentAsWideString();
		ArrayOf param2 = argIn[1];
		std::wstring fieldvalue;
		bool doReset = param2.isEmpty(true);
		if (!doReset)
		{
			fieldvalue = param2.getContentAsWideString();
		}
		ArrayOf previousvalue;
		if (fieldname == L"dwisdom")
		{
			previousvalue = ArrayOf::stringConstructor(getDoubleWisdomInformation());
			if (doReset)
			{
				resetDoubleWisdom();
			}
			else
			{
				if (!setDoubleWisdomInformation(fieldvalue))
				{
					Error(eval, _W("Cannot apply wisdom."));
				}
			}
		}
		else if (fieldname == L"swisdom")
		{
			previousvalue = ArrayOf::stringConstructor(getSingleWisdomInformation());
			if (doReset)
			{
				resetSingleWisdom();
			}
			else
			{
				if (!setSingleWisdomInformation(fieldvalue))
				{
					Error(eval, _W("Cannot apply wisdom."));
				}
			}
		}
		else if (fieldname == L"planner")
		{
			previousvalue = ArrayOf::stringConstructor(getPlannerInformation());
			if (doReset)
			{
				resetPlanner();
			}
			else
			{
				if (fieldvalue == L"estimate")
				{
					setPlannerInformation(FftPlannerMethod::ESTIMATE);
				}
				else if (fieldname == L"measure")
				{
					setPlannerInformation(FftPlannerMethod::MEASURE);
				}
				else if (fieldname == L"patient")
				{
					setPlannerInformation(FftPlannerMethod::PATIENT);
				}
				else if (fieldname == L"exhaustive")
				{
					setPlannerInformation(FftPlannerMethod::EXHAUSTIVE);
				}
				else if (fieldname == L"hybrid")
				{
					setPlannerInformation(FftPlannerMethod::HYBRID);
				}
				else
				{
					Error(eval, ERROR_WRONG_ARGUMENT_1_VALUE);
				}
			}
		}
		else
		{
			Error(eval, ERROR_WRONG_ARGUMENT_1_VALUE);
		}
		retval.push_back(previousvalue);

	}
	return retval;
}
//=============================================================================
