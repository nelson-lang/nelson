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
#include "historyBuiltin.hpp"
#include "Error.hpp"
#include "HistoryManager.hpp"
#include "IsCellOfStrings.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
historyBuiltin_size_one_rhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf arg1 = argIn[0];
    if (arg1.isRowVectorCharacterArray()) {
        std::wstring str = arg1.getContentAsWideString();
        HistoryManager* ptrHistoryManager = (HistoryManager*)eval->HistoryManager;
        if (str == L"size") {
            retval.push_back(
                ArrayOf::doubleConstructor((double)ptrHistoryManager->getLastNCommandsSize()));
        } else if (str == L"enable_save") {
            retval.push_back(ArrayOf::logicalConstructor(ptrHistoryManager->getSaveEnabled()));
        } else if (str == L"filename") {
            retval.push_back(ArrayOf::characterArrayConstructor(ptrHistoryManager->getFilename()));
        } else if (str == L"get") {
            wstringVector res = ptrHistoryManager->get();
            retval.push_back(ToCellStringAsColumn(res));
        } else if (str == L"display") {
            if (nLhs == 0) {
                wstringVector lines = ptrHistoryManager->get();
                if (lines.size() > 0) {
                    Interface* io = eval->getInterface();
                    if (io) {
                        for (size_t k = 0; k < lines.size(); k++) {
                            io->outputMessage(std::to_wstring(k) + L" : " + lines[k] + L"\n");
                        }
                    }
                }
            } else {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        } else if (str == L"save") {
            if (nLhs == 0) {
                ptrHistoryManager->saveToFile();
            } else {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        } else if (str == L"load") {
            if (nLhs == 0) {
                ptrHistoryManager->loadFromFile();
            } else {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        } else if (str == L"clear") {
            if (nLhs == 0) {
                ptrHistoryManager->clear(true);
            } else {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        } else if (str == L"duplicated") {
            retval.push_back(
                ArrayOf::logicalConstructor(ptrHistoryManager->getAllowDuplicatedLines()));
        } else if (str == L"saveafter") {
            retval.push_back(
                ArrayOf::doubleConstructor((double)ptrHistoryManager->getSaveAfterNCommands()));
        } else if (str == L"removeexit") {
            retval.push_back(ArrayOf::logicalConstructor(ptrHistoryManager->getRemoveExit()));
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_VALUE);
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
historyBuiltin_no_rhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    HistoryManager* ptrHistoryManager = (HistoryManager*)eval->HistoryManager;
    wstringVector res = ptrHistoryManager->get();
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        if (io) {
            for (size_t k = 0; k < res.size(); k++) {
                io->outputMessage(std::to_wstring(k + 1) + L" : " + res[k] + L"\n");
            }
        }
    } else {
        retval.push_back(ToCellStringAsColumn(res));
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
historyBuiltin_two_rhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf arg1 = argIn[0];
    ArrayOf arg2 = argIn[1];
    if (arg1.isRowVectorCharacterArray()) {
        std::wstring str = arg1.getContentAsWideString();
        HistoryManager* ptrHistoryManager = (HistoryManager*)eval->HistoryManager;
        if (str == L"size") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isScalar()) {
                if (arg2.getDataClass() == NLS_DOUBLE) {
                    double dvalue = arg2.getContentAsDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dvalue < 0) {
                        Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                    }
                    size_t ivalue = (size_t)dvalue;
                    if ((double)ivalue != dvalue) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    ptrHistoryManager->setLastNCommandsSize(ivalue);
                } else {
                    Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED);
            }
        } else if (str == L"enable_save") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isScalar()) {
                if (arg2.isLogical()) {
                    logical bEnable = arg2.getContentAsLogicalScalar();
                    ptrHistoryManager->setSaveEnabled(bEnable ? true : false);
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED);
            }
        } else if (str == L"delete") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.getDataClass() == NLS_DOUBLE) {
                if (arg2.isScalar()) {
                    double dvalue = arg2.getContentAsDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dvalue < 0) {
                        Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                    }
                    size_t ivalue = (size_t)dvalue;
                    if ((double)ivalue != dvalue) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    ptrHistoryManager->remove(ivalue);
                } else {
                    Dimensions sze(arg2.getDimensions());
                    Dimensions supported(1, 2);
                    if (sze.equals(supported)) {
                        double* dvalues = (double*)arg2.getDataPointer();
                        double dvalue1 = dvalues[0];
                        double dvalue2 = dvalues[1] + 1;
                        if (!std::isfinite(dvalue1) || !std::isfinite(dvalue2)) {
                            Error(ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (dvalue1 < 0 || dvalue2 < 0) {
                            Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                        }
                        size_t ivalue1 = (size_t)dvalue1;
                        size_t ivalue2 = (size_t)dvalue2;
                        if (((double)ivalue1 != dvalue1) || ((double)ivalue2 != dvalue2)) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (ivalue2 <= ivalue1) {
                            Error(ERROR_WRONG_ARGUMENT_2_A_MUST_BE_HIGHER_THAN_B);
                        }
                        if ((ivalue1 >= ptrHistoryManager->getCurrentSize())
                            || (ivalue2 >= ptrHistoryManager->getCurrentSize())) {
                            Error(ERROR_WRONG_ARGUMENT_2_A_MUST_BE_HIGHER_THAN_B);
                        } else {
                            ptrHistoryManager->remove(ivalue1, ivalue2);
                        }
                    } else {
                        Error(ERROR_WRONG_ARGUMENT_2_SIZE_A_B_VECTOR_EXPECTED);
                    }
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_DOUBLE_EXPECTED);
            }
        } else if (str == L"append") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring str = arg2.getContentAsWideString();
                HistoryManager* ptrHistoryManager = (HistoryManager*)eval->HistoryManager;
                ptrHistoryManager->appendLine(str);
            } else if (IsCellOfString(arg2)) {
                ArrayOf cell(arg2);
                ArrayOf* arg = (ArrayOf*)(cell.getDataPointer());
                for (indexType k = 0; k < arg2.getDimensions().getElementCount(); k++) {
                    ptrHistoryManager->appendLine(arg[k].getContentAsWideString());
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_OR_CELL_EXPECTED);
            }
        } else if (str == L"filename") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.getContentAsWideString();
                ptrHistoryManager->setFilename(filename);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else if (str == L"load") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.getContentAsWideString();
                ptrHistoryManager->loadFromFile(filename);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else if (str == L"save") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.getContentAsWideString();
                ptrHistoryManager->saveToFile(filename);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else if (str == L"duplicated") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isScalar()) {
                if (arg2.isLogical()) {
                    logical bDuplicated = arg2.getContentAsLogicalScalar();
                    ptrHistoryManager->setAllowDuplicatedLines(bDuplicated ? true : false);
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED);
            }
        } else if (str == L"removeexit") {
            if (nLhs > 0) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.isScalar()) {
                if (arg2.isLogical()) {
                    logical bRemove = arg2.getContentAsLogicalScalar();
                    ptrHistoryManager->setRemoveExit(bRemove ? true : false);
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED);
            }
        } else if (str == L"get") {
            if (nLhs > 1) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (arg2.getDataClass() == NLS_DOUBLE) {
                if (arg2.isScalar()) {
                    double dvalue = arg2.getContentAsDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dvalue < 0) {
                        Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                    }
                    size_t ivalue = (size_t)dvalue;
                    if ((double)ivalue != dvalue) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    std::wstring line = ptrHistoryManager->get(ivalue);
                    retval.push_back(ArrayOf::characterArrayConstructor(line));
                } else {
                    Dimensions sze(arg2.getDimensions());
                    Dimensions supported(1, 2);
                    if (sze.equals(supported)) {
                        double* dvalues = (double*)arg2.getDataPointer();
                        double dvalue1 = dvalues[0];
                        double dvalue2 = dvalues[1] + 1;
                        if (!std::isfinite(dvalue1) || !std::isfinite(dvalue2)) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (dvalue1 < 0 || dvalue2 < 0) {
                            Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                        }
                        size_t ivalue1 = (size_t)dvalue1;
                        size_t ivalue2 = (size_t)dvalue2;
                        if (((double)ivalue1 != dvalue1) || ((double)ivalue2 != dvalue2)) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (ivalue2 <= ivalue1) {
                            Error(ERROR_WRONG_ARGUMENT_2_A_MUST_BE_HIGHER_THAN_B);
                        }
                        if ((ivalue1 >= ptrHistoryManager->getCurrentSize())
                            || (ivalue2 >= ptrHistoryManager->getCurrentSize())) {
                            Error(ERROR_WRONG_ARGUMENT_2_A_MUST_BE_HIGHER_THAN_B);
                        } else {
                            wstringVector res = ptrHistoryManager->get(ivalue1, ivalue2);
                            retval.push_back(ToCellStringAsColumn(res));
                        }
                    } else {
                        Error(ERROR_WRONG_ARGUMENT_2_SIZE_A_B_VECTOR_EXPECTED);
                    }
                }
            } else if (str == L"saveafter") {
                if (nLhs > 0) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                if (arg2.isScalar()) {
                    if (arg2.getDataClass() == NLS_DOUBLE) {
                        double dvalue = arg2.getContentAsDoubleScalar();
                        if (!std::isfinite(dvalue)) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (dvalue < 0) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        size_t ivalue = (size_t)dvalue;
                        if ((double)ivalue != dvalue) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        ptrHistoryManager->setSaveAfterNCommands(ivalue);
                    } else {
                        Error(ERROR_WRONG_ARGUMENT_2_TYPE_DOUBLE_EXPECTED);
                    }
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_VALUE);
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::HistoryManagerGateway::historyBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (eval->HistoryManager == nullptr) {
        Error(_W("History manager not enabled."));
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size()) {
    case 0: {
        return historyBuiltin_no_rhs(eval, nLhs, argIn);
    } break;
    case 1: {
        return historyBuiltin_size_one_rhs(eval, nLhs, argIn);
    } break;
    case 2: {
        return historyBuiltin_two_rhs(eval, nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
