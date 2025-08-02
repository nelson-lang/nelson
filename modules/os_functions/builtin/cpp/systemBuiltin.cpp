//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "systemBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "SystemCommand.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::systemBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);
    ArrayOfVector retval;
    std::vector<uint64> timeouts;
    bool bEcho = false;
    wstringVector commands;
    bool outputAsArray = false;

    if (argIn[0].isStringArray() || argIn[0].isCellArrayOfCharacterVectors()) {
        commands = argIn[0].getContentAsWideStringVector(true);
        outputAsArray = true;
    } else if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring command = argIn[0].getContentAsWideString();
        commands.push_back(command);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }

    if (argIn.size() > 1) {
        if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            std::wstring flag = argIn[1].getContentAsWideString();
            if (flag.compare(L"-echo") == 0) {
                bEcho = true;
            } else {
                Error(_W("Unrecognized option. \"-echo\" or timeout value expected."));
            }
        } else if (argIn[1].isNumeric()) {
            if (argIn[1].isScalar()) {
                uint64 timeout = argIn[1].getContentAsUnsignedInteger64Scalar();
                if (commands.size() == 1) {
                    timeouts.push_back(timeout);
                } else {
                    for (size_t k = 0; k < commands.size(); ++k) {
                        timeouts.push_back(timeout);
                    }
                }
            } else if (argIn[1].isVector()) {
                if (argIn[1].getElementCount() == commands.size()) {
                    ArrayOf asUint64 = argIn[1];
                    asUint64.promoteType(NLS_UINT64);
                    uint64* ptrUint64 = (uint64*)asUint64.getDataPointer();
                    for (size_t k = 0; k < commands.size(); ++k) {
                        timeouts.push_back(ptrUint64[k]);
                    }
                } else {
                    Error(_W("same size expected."));
                }
            }
        } else {
            Error(_W("Unrecognized option. \"-echo\" or timeout value expected."));
        }
    }

    if (nLhs == 0 && commands.size() == 1) {
        bEcho = true;
    }

    if (bEcho && commands.size() > 1) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }

    if (timeouts.empty()) {
        for (size_t k = 0; k < commands.size(); ++k) {
            timeouts.push_back((uint64)0);
        }
    }
    std::vector<std::tuple<int, std::wstring, uint64>> results
        = ParallelSystemCommand(commands, timeouts, eval->haveEventsLoop(), eval->getID());

    if (results.size() == 1 && !outputAsArray) {
        std::tuple<int, std::wstring, uint64> result = results[0];
        if (bEcho && eval) {
            Interface* io = eval->getInterface();
            if (io) {
                io->outputMessage(std::get<1>(result));
            }
        }

        retval << ArrayOf::doubleConstructor(static_cast<double>(std::get<0>(result)));
        if (nLhs > 1) {
            retval << ArrayOf::characterArrayConstructor(std::get<1>(result));
        }
        if (nLhs > 2) {
            retval << ArrayOf::doubleConstructor(static_cast<double>(std::get<2>(result)));
        }
    } else {
        Dimensions dimsOut(1, results.size());
        double* pdStatus = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, results.size());
        for (size_t k = 0; k < results.size(); ++k) {
            pdStatus[k] = (double)std::get<0>(results[k]);
        }
        retval << ArrayOf(NLS_DOUBLE, dimsOut, pdStatus);
        if (nLhs > 1) {
            ArrayOf* pArray = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, results.size());
            ArrayOf outRet = ArrayOf(NLS_STRING_ARRAY, dimsOut, pArray);
            size_t i = 0;
            for (auto k : results) {
                pArray[i++] = ArrayOf::characterArrayConstructor(std::get<1>(k));
            }
            retval << outRet;
        }
        if (nLhs > 2) {
            double* pdDuration = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, results.size());
            size_t i = 0;
            for (auto k : results) {
                pdDuration[i++] = (double)std::get<2>(k);
            }
            retval << ArrayOf(NLS_DOUBLE, dimsOut, pdDuration);
        }
    }
    return retval;
}
//=============================================================================
