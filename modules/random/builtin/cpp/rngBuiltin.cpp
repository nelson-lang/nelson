//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rngBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Rng.hpp"
#include "Rng_helpers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
backupCurrentRng();
static ArrayOfVector
splitRngStruct(const ArrayOf& structRng);
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::rngBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 0: {
        if (!haveRandomEngine()) {
            Error(_W("random engine not initialized."));
        }
        retval << backupCurrentRng();
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
                backupCurrentRngStruct = backupCurrentRng();
            }
            if (param == L"default") {
                RngSetDefault();
                if (nLhs == 1) {
                    retval << backupCurrentRngStruct;
                }
            } else if (param == L"shuffle") {
                RngShuffle();
                if (nLhs == 1) {
                    retval << backupCurrentRngStruct;
                }
            } else if (param == L"enginelist") {
                retval << ArrayOf::toCellArrayOfCharacterColumnVectors(getSupportedRngEngineName());
            }
        } else if (arg1.isNumeric()) {
            double s = arg1.getContentAsDoubleScalar();
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = backupCurrentRng();
            }
            RngSetSeed(s);
            if (nLhs == 1) {
                retval << backupCurrentRngStruct;
            }
        } else if (arg1.isStruct()) {
            ArrayOfVector elements = splitRngStruct(arg1);
            std::wstring genname = elements[0].getContentAsWideString();
            double s = elements[1].getContentAsDoubleScalar();
            RngSetEngine(s, genname);
            RngSetState(elements[2]);
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
                backupCurrentRngStruct = backupCurrentRng();
            }
            RngSetEngine(s, genname);
            if (nLhs == 1) {
                retval << backupCurrentRngStruct;
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
                backupCurrentRngStruct = backupCurrentRng();
            }
            RngSetEngine(0, genname);
            RngShuffle();
            if (nLhs == 1) {
                retval << backupCurrentRngStruct;
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
backupCurrentRng()
{
    ArrayOf typeOfRng = ArrayOf::characterArrayConstructor(RngGetType());
    ArrayOf seedOfRng = RngGetSeed();
    ArrayOf stateOfRng = RngGetState();
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
splitRngStruct(const ArrayOf& structRng)
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
