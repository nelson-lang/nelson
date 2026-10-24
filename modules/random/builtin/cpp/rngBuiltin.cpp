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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Rng.hpp"
#include "Rng_helpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// lst = rng('enginelist')
// rng('default')
// s = rng('default')
// rng('shuffle')
// s = rng('shuffle')
// rng(seed)
// s = rng(seed)
// rng(seed, generator)
// s = rng(seed, generator)
// rng('shuffle', generator)
// s = rng('shuffle', generator)
// s = rng
// rng(s)
//=============================================================================
ArrayOfVector
Nelson::RandomGateway::rngBuiltin(int nLhs, const ArrayOfVector& argInRaw)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    // String scalars (rng(1,"twister")) are converted to character arrays up front:
    // the code below only tests isRowVectorCharacterArray().
    ArrayOfVector argIn = argInRaw;
    for (size_t k = 0; k < argIn.size(); ++k) {
        if (argIn[k].isScalarStringArray()) {
            argIn[k] = ArrayOf::characterArray(argIn[k].asWideString());
        }
    }
    switch (argIn.size()) {
    case 0: {
        if (!haveRandomEngine()) {
            Error(L"Nelson:rng:engineNotInitialized", L"random engine not initialized.");
        }
        retval << RngGetSettings();
    } break;
    case 1: {
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring param = arg1.asWideString();
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = RngGetSettings();
            }
            if (isRngType(param)) {
                RngSetEngine(0, param);
                if (nLhs == 1) {
                    retval << backupCurrentRngStruct;
                }
                return retval;
            }
            if (!((param == L"default") || (param == L"shuffle") || (param == L"enginelist"))) {
                Error(L"Nelson:rng:invalidOption", L"%s expected.",
                    L"'default', 'shuffle' or 'enginelist'");
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
                retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
                    getSupportedRngEngineName());
            }
        } else if (arg1.isNumeric()) {
            double s = arg1.asDoubleScalar();
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = RngGetSettings();
            }
            RngSetSeed(s);
            if (nLhs == 1) {
                retval << backupCurrentRngStruct;
            }
        } else if (arg1.isStruct()) {
            RngSetSettings(arg1);
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string, numeric value or struct");
        }
    } break;
    case 2: {
        ArrayOf arg1 = argIn[0];
        ArrayOf arg2 = argIn[1];
        if (arg1.isNumeric() && arg2.isRowVectorCharacterArray()) {
            double s = arg1.asDoubleScalar();
            std::wstring genname = arg2.asWideString();
            if (!isRngType(genname)) {
                Error(L"Nelson:rng:invalidGeneratorName", L"A valid generator name expected.");
            }
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = RngGetSettings();
            }
            RngSetEngine(s, genname);
            if (nLhs == 1) {
                retval << backupCurrentRngStruct;
            }
        } else if (arg1.isRowVectorCharacterArray() && arg2.isRowVectorCharacterArray()) {
            std::wstring param = arg1.asWideString();
            if (param != L"shuffle") {
                Error(L"Nelson:rng:invalidOption:valueExpected", L"'%s' expected.", L"shuffle");
            }
            std::wstring genname = arg2.asWideString();
            if (!isRngType(genname)) {
                Error(L"Nelson:rng:invalidGeneratorName", L"A valid generator name expected.");
            }
            ArrayOf backupCurrentRngStruct;
            if (nLhs == 1) {
                backupCurrentRngStruct = RngGetSettings();
            }
            RngSetEngine(0, genname);
            RngShuffle();
            if (nLhs == 1) {
                retval << backupCurrentRngStruct;
            }
        } else {
            if (!arg2.isRowVectorCharacterArray()) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"string");
            }
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"numeric value or string");
        }
    } break;
    default: {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
