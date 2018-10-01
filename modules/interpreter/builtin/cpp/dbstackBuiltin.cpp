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
#include "dbstackBuiltin.hpp"
#include "DebugStack.hpp"
#include "Error.hpp"
#include "PositionScript.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
checkArgument(Evaluator* eval, ArrayOf arg, bool& withCompleteNames, int& nbOmits,
    bool& isCompleteNames, bool& isNbOmits)
{
    if (arg.isRowVectorCharacterArray()) {
        std::wstring str = arg.getContentAsWideString();
        if (str != L"-completenames") {
            Error(_W("'-completenames' expected."));
        }
        withCompleteNames = true;
        isCompleteNames = true;
    } else if (arg.isScalar()) {
        ArrayOf param1 = arg;
        double value = 0;
        if (param1.isIntegerType()) {
            param1.promoteType(NLS_DOUBLE);
        }
        switch (param1.getDataClass()) {
        case NLS_DOUBLE: {
            value = param1.getContentAsDoubleScalar();
        } break;
        case NLS_SINGLE: {
            value = (double)param1.getContentAsSingleScalar();
        } break;
        default: {
        } break;
            Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
        }
        int intValue = (int)value;
        if ((double)intValue != value) {
            Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
        }
        nbOmits = intValue + 1;
        isNbOmits = true;
    } else {
        Error(_W("'-completenames' expected or scalar integer value required."));
    }
}
//=============================================================================
static std::wstring
shortName(std::wstring filename)
{
    boost::filesystem::path p(filename);
    return p.filename().generic_wstring();
}
//=============================================================================
static ArrayOf
dbstackAsStruct(stackTrace positions, bool withCompleteNames)
{
    stringVector fieldnames;
    ArrayOf st;
    fieldnames.push_back("file");
    fieldnames.push_back("name");
    fieldnames.push_back("line");
    Dimensions dims;
    dims[0] = positions.size();
    dims[1] = 1;
    if (positions.empty()) {
        st = ArrayOf::emptyStructConstructor(fieldnames, dims);
    } else {
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames);
        st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        ArrayOfVector file;
        ArrayOfVector name;
        ArrayOfVector line;
        file.reserve(positions.size());
        name.reserve(positions.size());
        line.reserve(positions.size());
        for (size_t k = 0; k < positions.size(); ++k) {
            std::wstring filename = positions[k].getFilename();
            if (!withCompleteNames) {
                filename = shortName(positions[k].getFilename());
            }
            file.push_back(ArrayOf::characterArrayConstructor(filename));
            std::wstring functionName = positions[k].getFunctionName();
            name.push_back(ArrayOf::characterArrayConstructor(functionName));
            if (positions[k].getLine() == 0) {
                line.push_back(ArrayOf::emptyConstructor(0, 1));
            } else {
                line.push_back(ArrayOf::doubleConstructor((double)positions[k].getLine()));
            }
        }
        st.setFieldAsList("file", file);
        st.setFieldAsList("name", name);
        st.setFieldAsList("line", line);
    }
    return st;
}
//=============================================================================
void
dbstackPrint(Interface* io, stackTrace positions, bool withCompleteNames)
{
    for (size_t k = 0; k < positions.size(); k++) {
        std::wstring message;
        std::wstring filename = positions[k].getFilename();
        if (!withCompleteNames) {
            filename = shortName(positions[k].getFilename());
        }
        if (positions[k].getLine() == 0) {
            if (filename != L"") {
                message = std::wstring(L"In ") + filename + L"\n";
            }
        } else {
            if (positions[k].getFunctionName() != L"") {
                message = std::wstring(L"In ") + filename + L" function "
                    + positions[k].getFunctionName() + L" (line "
                    + std::to_wstring(positions[k].getLine()) + L")\n";
            } else {
                message = std::wstring(L"In ") + filename + L" (line "
                    + std::to_wstring(positions[k].getLine()) + L")\n";
            }
        }
        io->outputMessage(message);
    }
    io->outputMessage(L"\n");
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::dbstackBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    int nbOmits = 1;
    bool withCompleteNames = false;
    stackTrace positions;
    switch (argIn.size()) {
    case 0: {
    } break;
    case 1: {
        bool isCompleteNames = false;
        bool isNbOmits = false;
        bool _withCompleteNames = false;
        int _nbOmits = 1;
        checkArgument(eval, argIn[0], _withCompleteNames, _nbOmits, isCompleteNames, isNbOmits);
        if (isCompleteNames) {
            withCompleteNames = isCompleteNames;
        }
        if (isNbOmits) {
            nbOmits = _nbOmits;
        }
    } break;
    case 2: {
        bool isCompleteNames = false;
        bool isNbOmits = false;
        bool _withCompleteNames = false;
        int _nbOmits = 1;
        bool _wasWithCompleteNames = false;
        bool _wasNbOmits = false;
        checkArgument(eval, argIn[0], _withCompleteNames, _nbOmits, isCompleteNames, isNbOmits);
        if (isCompleteNames) {
            withCompleteNames = isCompleteNames;
        }
        if (isNbOmits) {
            nbOmits = _nbOmits;
        }
        checkArgument(eval, argIn[1], _withCompleteNames, _nbOmits, isCompleteNames, isNbOmits);
        if (isCompleteNames) {
            withCompleteNames = isCompleteNames;
        }
        if (isNbOmits) {
            nbOmits = _nbOmits;
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    DebugStack(eval->cstack, nbOmits, positions);

    switch (nLhs) {
    case 0: {
        if (eval) {
            Interface* io = eval->getInterface();
            if (io) {
                dbstackPrint(io, positions, withCompleteNames);
            }
        }
    } break;
    case 1: {
        retval.push_back(dbstackAsStruct(positions, withCompleteNames));
    } break;
    case 2: {
        retval.push_back(dbstackAsStruct(positions, withCompleteNames));
        double indexWorkspace = (double)positions.size();
        if (indexWorkspace <= 0) {
            indexWorkspace = 1;
        }
        retval.push_back(ArrayOf::doubleConstructor(indexWorkspace));
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
