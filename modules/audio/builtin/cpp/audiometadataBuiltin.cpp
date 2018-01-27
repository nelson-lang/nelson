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
#include "audiometadataBuiltin.hpp"
#include "Error.hpp"
#include "AudioFileMetaData.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// info = audiometadata(filename)
//=============================================================================
ArrayOfVector Nelson::AudioGateway::audiometadataBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    std::wstring errorMessage;
    ArrayOf param1 = argIn[0];
    std::wstring filename = param1.getContentAsWideString();
    wstringVector outputMetaData;
    if (argIn.size() == 1)
    {
        outputMetaData = AudioFileMetaData(filename, errorMessage);
    }
    if (errorMessage != L"")
    {
        Error(eval, errorMessage);
    }
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back("Title");
    if (outputMetaData[0] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[0]));
    }
    fieldnames.push_back("Comment");
    if (outputMetaData[1] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[1]));
    }
    fieldnames.push_back("Artist");
    if (outputMetaData[2] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[2]));
    }
    fieldnames.push_back("Copyright");
    if (outputMetaData[3] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[3]));
    }
    fieldnames.push_back("Software");
    if (outputMetaData[4] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[4]));
    }
    fieldnames.push_back("Date");
    if (outputMetaData[5] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[5]));
    }
    fieldnames.push_back("Album");
    if (outputMetaData[6] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[6]));
    }
    fieldnames.push_back("License");
    if (outputMetaData[7] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[7]));
    }
    fieldnames.push_back("TrackNumber");
    if (outputMetaData[8] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[8]));
    }
    fieldnames.push_back("Genre");
    if (outputMetaData[9] == L"")
    {
        fieldvalues.push_back(ArrayOf::emptyConstructor());
    }
    else
    {
        fieldvalues.push_back(ArrayOf::stringConstructor(outputMetaData[9]));
    }
    retval.push_back(ArrayOf::structConstructor(fieldnames, fieldvalues));
    return retval;
}
//=============================================================================
