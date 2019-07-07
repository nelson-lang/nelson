//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <iomanip>
#include "profileBuiltin.hpp"
#include "Error.hpp"
#include "Profiler.hpp"
#include "ModulesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static Profiler::Profile_Sort_Type
getSortArgument(const ArrayOfVector& argIn, bool& validOption)
{
    Profiler::Profile_Sort_Type sortOption = Profiler::Profile_Sort_Type::SORT_BY_NAMEFILELINE;
    if (argIn.size() > 1) {
        ArrayOf param2 = argIn[1];
        std::wstring str = param2.getContentAsWideString();
        validOption = false;
        if (str == L"nfl") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_NAMEFILELINE;
            validOption = true;
        }
        if (str == L"line") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_LINE;
            validOption = true;
        }
        if (str == L"percalls") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_PERCALL;
            validOption = true;
        }
        if (str == L"totaltime") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_TOTALTIME;
            validOption = true;
        }
        if (str == L"filename") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_FILENAME;
            validOption = true;
        }
        if (str == L"function") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_NAME;
            validOption = true;
        }
        if (str == L"nbcalls") {
            sortOption = Profiler::Profile_Sort_Type::SORT_BY_NBCALLS;
            validOption = true;
        }
        if (!validOption) {
            Error(_W("option not managed."));
        }
    }
    return sortOption;
}
//=============================================================================
ArrayOfVector
Nelson::ProfilerGateway::profileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 1 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::string arg1AsString = param1.getContentAsCString();
    bool validOption = false;
    if (arg1AsString == "on") {
        if (argIn.size() > 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        Profiler::getInstance()->on();
        validOption = true;
    }
    if (arg1AsString == "off") {
        if (argIn.size() > 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        Profiler::getInstance()->off();
        validOption = true;
    }
    if (arg1AsString == "resume") {
        if (argIn.size() > 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        Profiler::getInstance()->resume();
        validOption = true;
    }
    if (arg1AsString == "clear") {
        if (argIn.size() > 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        Profiler::getInstance()->clear();
        validOption = true;
    }

    if (arg1AsString == "info") {
        if (argIn.size() > 2) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        Profiler::Profile_Sort_Type sortOption = getSortArgument(argIn, validOption);
        std::vector<std::tuple<std::string, uint64, std::string, uint64, uint64, uint64>>
            profileLines = Profiler::getInstance()->info(sortOption);

        size_t nbElements = profileLines.size();
        ArrayOfVector FunctionName;
        ArrayOfVector Filename;
        ArrayOfVector LinePosition;
        ArrayOfVector NumCalls;
        ArrayOfVector TotalTime;
        ArrayOfVector PerCall;

        FunctionName.reserve(nbElements);
        Filename.reserve(nbElements);
        LinePosition.reserve(nbElements);
        NumCalls.reserve(nbElements);
        TotalTime.reserve(nbElements);
        PerCall.reserve(nbElements);
        // filename, line, name, nbcalls, tottime, percall
        for (std::tuple<std::string, uint64, std::string, uint64, uint64, uint64> element :
            profileLines) {
            FunctionName.push_back(ArrayOf::characterArrayConstructor(std::get<2>(element)));
            Filename.push_back(ArrayOf::characterArrayConstructor(std::get<0>(element)));
            LinePosition.push_back(ArrayOf::doubleConstructor((double)std::get<1>(element)));
            NumCalls.push_back(ArrayOf::doubleConstructor((double)std::get<3>(element)));
            double totalTimeAsSeconds = (double)std::get<4>(element) * 1e-9;
            TotalTime.push_back(ArrayOf::doubleConstructor(totalTimeAsSeconds));
            double perCallAsSeconds = (double)std::get<5>(element) * 1e-9;
            PerCall.push_back(ArrayOf::doubleConstructor(perCallAsSeconds));
        }
        stringVector fieldnames;
        fieldnames.push_back("FunctionName");
        fieldnames.push_back("Filename");
        fieldnames.push_back("LinePosition");
        fieldnames.push_back("NumCalls");
        fieldnames.push_back("TotalTime");
        fieldnames.push_back("PerCall");

        Dimensions dims(nbElements, 1);
        ArrayOf* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, nbElements, fieldnames, false));
        ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);

        st.setFieldAsList("FunctionName", FunctionName);
        st.setFieldAsList("Filename", Filename);
        st.setFieldAsList("LinePosition", LinePosition);
        st.setFieldAsList("NumCalls", NumCalls);
        st.setFieldAsList("TotalTime", TotalTime);
        st.setFieldAsList("PerCall", PerCall);
        retval.push_back(st);
        validOption = true;
    }

    if (arg1AsString == "show") {
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        int nbLinesToDisplay = -1;
        if (argIn.size() > 2) {
            ArrayOf param3 = argIn[2];
            nbLinesToDisplay = param3.getContentAsInteger32Scalar(true);
            if (nbLinesToDisplay < 1) {
                Error(_W("Invalid value."));
            }
        }

        Profiler::Profile_Sort_Type sortOption = getSortArgument(argIn, validOption);
        Profiler::getInstance()->show(eval->getInterface(), sortOption, nbLinesToDisplay);
        validOption = true;
    }

    if (arg1AsString == "status") {
        if (argIn.size() > 1) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        stringVector fieldnames;
        fieldnames.push_back("ProfilerStatus");
        fieldnames.push_back("HistoryTracking");
        fieldnames.push_back("HistorySize");
        ArrayOfVector values;
        if (Profiler::getInstance()->isOn()) {
            values.push_back(ArrayOf::characterArrayConstructor("on"));
        } else {
            values.push_back(ArrayOf::characterArrayConstructor("off"));
        }
        values.push_back(ArrayOf::characterArrayConstructor("off"));
        values.push_back(ArrayOf::doubleConstructor(1));
        retval.push_back(ArrayOf::structScalarConstructor(fieldnames, values));
        validOption = true;
    }

    if (!validOption) {
        Error(_W("option not managed."));
    }
    return retval;
}
//=============================================================================
