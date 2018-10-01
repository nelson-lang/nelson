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
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
#include "pauseBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool pauseOn = true;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::pauseBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (argIn.size() == 0) {
        if (nLhs == 1) {
            if (pauseOn) {
                retval.push_back(ArrayOf::characterArrayConstructor(L"on"));
            } else {
                retval.push_back(ArrayOf::characterArrayConstructor(L"off"));
            }
            return retval;
        }
        Interface* io = eval->getInterface();
        if (io) {
            io->getInput(L"");
        }
    } else {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring arg1Value = param1.getContentAsWideString();
            if (arg1Value == L"on" || arg1Value == L"off" || arg1Value == L"query") {
                bool previousValue = pauseOn;
                if (arg1Value == L"on") {
                    pauseOn = true;
                    if (nLhs == 1) {
                        if (previousValue) {
                            retval.push_back(ArrayOf::characterArrayConstructor(L"on"));
                        } else {
                            retval.push_back(ArrayOf::characterArrayConstructor(L"off"));
                        }
                        return retval;
                    }
                } else if (arg1Value == L"off") {
                    pauseOn = false;
                    if (nLhs == 1) {
                        if (previousValue) {
                            retval.push_back(ArrayOf::characterArrayConstructor(L"on"));
                        } else {
                            retval.push_back(ArrayOf::characterArrayConstructor(L"off"));
                        }
                        return retval;
                    }
                } else {
                    if (pauseOn) {
                        retval.push_back(ArrayOf::characterArrayConstructor(L"on"));
                    } else {
                        retval.push_back(ArrayOf::characterArrayConstructor(L"off"));
                    }
                    return retval;
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
        } else if (param1.isNumeric()) {
            if (nLhs == 1) {
                if (pauseOn) {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"on"));
                } else {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"off"));
                }
                return retval;
            } else {
                double val = param1.getContentAsDoubleScalar();
                if (!pauseOn) {
                    return retval;
                }
                if (std::isinf(val)) {
                    while (!NelsonConfiguration::getInstance()->getInterruptPending()) {
                        boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(10)));
                        if (eval->haveEventsLoop()) {
                            ProcessEventsDynamicFunctionWithoutWait();
                        }
                    }
                } else if (std::isnan(val)) {
                    // DO NOTHING
                } else {
                    boost::chrono::nanoseconds begin_time
                        = boost::chrono::high_resolution_clock::now().time_since_epoch();
                    bool bContinue = true;
                    do {
                        boost::this_thread::sleep_for(boost::chrono::nanoseconds(uint64(10)));
                        boost::chrono::nanoseconds current_time
                            = boost::chrono::high_resolution_clock::now().time_since_epoch();
                        boost::chrono::nanoseconds difftime = (current_time - begin_time);
                        bContinue = !(difftime.count() > int64(val * 1e9));
                        if (eval->haveEventsLoop()) {
                            ProcessEventsDynamicFunctionWithoutWait();
                        }
                    } while (!NelsonConfiguration::getInstance()->getInterruptPending()
                        && (bContinue == true));
                }
            }
        } else {
            bool bSuccess = false;
            retval = OverloadFunction(eval, nLhs, argIn, "pause", bSuccess);
            if (!bSuccess) {
                OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION, "pause");
            }
        }
    }
    return retval;
}
//=============================================================================
