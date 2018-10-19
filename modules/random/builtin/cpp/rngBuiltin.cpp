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
#include "rngBuiltin.hpp"
#include "Error.hpp"
#include "Rng.hpp"
#include "Rng_helpers.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// lst = rng('enginelist') implemented
// rng('default') implemented
// s = rng('default') implemented
// rng('shuffle') implemented
// s = rng('shuffle') implemented
// rng(seed) implemented
// s = rng(seed) implemented
// rng(seed, generator) implemented
// s = rng(seed, generator) implemented
// rng('shuffle', generator) implemented
// s = rng('shuffle', generator) implemented
// s = rng implemented
// rng(s) implemented
//=============================================================================
static ArrayOf
backupCurrentRng(Evaluator* eval);
static ArrayOfVector
splitRngStruct(Evaluator* eval, ArrayOf structRng);
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::rngBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size()) {
    case 0: {
        if (eval->RandomEngine == nullptr) {
            Error(_W("random engine not initialized."));
        }
        retval.push_back(backupCurrentRng(eval));
    } break;
    case 1: {
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring param = arg1.getContentAsWideString();
            if (!((param == L"default") || (param == L"shuffle") || (param == L"enginelist"))) {
                Error(_W("'default', 'shuffle' or 'enginelist' expected."));
            }
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = backupCurrentRng(eval);
            }
            if (param == L"default") {
                RngSetDefault(eval);
                if (nLhs == 1) {
                    retval.push_back(backupCurrentRngStruct);
                }
            } else if (param == L"shuffle") {
                RngShuffle(eval);
                if (nLhs == 1) {
                    retval.push_back(backupCurrentRngStruct);
                }
            } else if (param == L"enginelist") {
                wstringVector enginelist = getSupportedRngEngineName();
                retval.push_back(ToCellStringAsColumn(enginelist));
            }
        } else if (arg1.isNumeric()) {
            double s = arg1.getContentAsDoubleScalar();
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = backupCurrentRng(eval);
            }
            RngSetSeed(eval, s);
            if (nLhs == 1) {
                retval.push_back(backupCurrentRngStruct);
            }
        } else if (arg1.isStruct()) {
            ArrayOfVector elements = splitRngStruct(eval, arg1);
            std::wstring genname = elements[0].getContentAsWideString();
            double s = elements[1].getContentAsDoubleScalar();
            RngSetEngine(eval, s, genname);
            RngSetState(eval, elements[2]);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE);
        }
    } break;
    case 2: {
        ArrayOf arg1 = argIn[0];
        ArrayOf arg2 = argIn[1];
        if (arg1.isNumeric() && arg2.isRowVectorCharacterArray()) {
            double s = arg1.getContentAsDoubleScalar();
            std::wstring genname = arg2.getContentAsWideString();
            if (!isRngType(genname)) {
                Error(_W("A valid generator name expected."));
            }
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = backupCurrentRng(eval);
            }
            RngSetEngine(eval, s, genname);
            if (nLhs == 1) {
                retval.push_back(backupCurrentRngStruct);
            }
        } else if (arg1.isRowVectorCharacterArray() && arg2.isRowVectorCharacterArray()) {
            std::wstring param = arg1.getContentAsWideString();
            if (param != L"shuffle") {
                Error(_W("'shuffle' expected."));
            }
            std::wstring genname = arg2.getContentAsWideString();
            if (!isRngType(genname)) {
                Error(_W("A valid generator name expected."));
            }
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = backupCurrentRng(eval);
            }
            RngSetEngine(eval, 0, genname);
            RngShuffle(eval);
            if (nLhs == 1) {
                retval.push_back(backupCurrentRngStruct);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENTS_TYPE);
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
static ArrayOf
backupCurrentRng(Evaluator* eval)
{
    ArrayOf typeOfRng = ArrayOf::characterArrayConstructor(RngGetType(eval));
    ArrayOf seedOfRng = RngGetSeed(eval);
    ArrayOf stateOfRng = RngGetState(eval);
    ArrayOf structRng;
    ArrayOfVector fieldvalues;
    stringVector fieldnames;
    fieldvalues.push_back(typeOfRng);
    fieldvalues.push_back(seedOfRng);
    fieldvalues.push_back(stateOfRng);
    fieldnames.push_back("Type");
    fieldnames.push_back("Seed");
    fieldnames.push_back("State");
    ArrayOf retStruct = ArrayOf::structConstructor(fieldnames, fieldvalues);
    return retStruct;
}
//=============================================================================
static ArrayOfVector
splitRngStruct(Evaluator* eval, ArrayOf structRng)
{
    ArrayOfVector elements;
    stringVector fieldnames = structRng.getFieldNames();
    if (fieldnames.size() == 3) {
        if ((fieldnames[0] == "Type") && (fieldnames[1] == "Seed") && (fieldnames[2] == "State")) {
            ArrayOf nType = structRng.getField("Type");
            ArrayOf nSeed = structRng.getField("Seed");
            ArrayOf nState = structRng.getField("State");
            elements.push_back(nType);
            elements.push_back(nSeed);
            elements.push_back(nState);
            return elements;
        }
    }
    Error(_W("Must contain generator settings captured previously."));
    return elements;
}
//=============================================================================
