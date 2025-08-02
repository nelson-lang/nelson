//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "historyBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HistoryManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
historyBuiltin_size_one_rhs(
    Evaluator* eval, HistoryManager* ptrHistoryManager, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf arg1 = argIn[0];
    if (arg1.isRowVectorCharacterArray()) {
        std::wstring str = arg1.getContentAsWideString();
        if (str == L"size") {
            retval << ArrayOf::doubleConstructor(
                static_cast<double>(ptrHistoryManager->getLastNCommandsSize()));
        } else if (str == L"enable_save") {
            retval << ArrayOf::logicalConstructor(ptrHistoryManager->getSaveEnabled());
        } else if (str == L"filename") {
            retval << ArrayOf::characterArrayConstructor(ptrHistoryManager->getFilename());
        } else if (str == L"get") {
            wstringVector res = ptrHistoryManager->get();
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(res);
        } else if (str == L"display") {
            if (nLhs == 0) {
                wstringVector lines = ptrHistoryManager->get();
                if (!lines.empty()) {
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
            retval << ArrayOf::logicalConstructor(ptrHistoryManager->getAllowDuplicatedLines());
        } else if (str == L"saveafter") {
            retval << ArrayOf::doubleConstructor(
                static_cast<double>(ptrHistoryManager->getSaveAfterNCommands()));
        } else if (str == L"removeexit") {
            retval << ArrayOf::logicalConstructor(ptrHistoryManager->getRemoveExit());
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
historyBuiltin_no_rhs(
    Evaluator* eval, HistoryManager* ptrHistoryManager, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    wstringVector res = ptrHistoryManager->get();
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        if (io) {
            for (size_t k = 0; k < res.size(); k++) {
                io->outputMessage(std::to_wstring(k + 1) + L" : " + res[k] + L"\n");
            }
        }
    } else {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(res);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
historyBuiltin_two_rhs(HistoryManager* ptrHistoryManager, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf arg1 = argIn[0];
    ArrayOf arg2 = argIn[1];
    if (arg1.isRowVectorCharacterArray()) {
        std::wstring str = arg1.getContentAsWideString();
        if (str == L"size") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isScalar()) {
                if (arg2.getDataClass() == NLS_DOUBLE) {
                    double dvalue = arg2.getContentAsDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dvalue < 0) {
                        Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
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
            nargoutcheck(nLhs, 0, 0);
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
            nargoutcheck(nLhs, 0, 0);
            if (arg2.getDataClass() == NLS_DOUBLE) {
                if (arg2.isScalar()) {
                    double dvalue = arg2.getContentAsDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dvalue < 0) {
                        Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    ptrHistoryManager->remove(ivalue);
                } else {
                    Dimensions sze(arg2.getDimensions());
                    Dimensions supported(1, 2);
                    if (sze.equals(supported)) {
                        auto* dvalues = (double*)arg2.getDataPointer();
                        double dvalue1 = dvalues[0];
                        double dvalue2 = dvalues[1] + 1;
                        if (!std::isfinite(dvalue1) || !std::isfinite(dvalue2)) {
                            Error(ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (dvalue1 < 0 || dvalue2 < 0) {
                            Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                        }
                        auto ivalue1 = static_cast<size_t>(dvalue1);
                        auto ivalue2 = static_cast<size_t>(dvalue2);
                        if ((static_cast<double>(ivalue1) != dvalue1)
                            || (static_cast<double>(ivalue2) != dvalue2)) {
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
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                ptrHistoryManager->appendLine(arg2.getContentAsWideString());
            } else if (arg2.isCellArrayOfCharacterVectors()) {
                ArrayOf cell(arg2);
                auto* arg = (ArrayOf*)(cell.getDataPointer());
                indexType elementCount = arg2.getElementCount();
                for (indexType k = 0; k < elementCount; k++) {
                    ptrHistoryManager->appendLine(arg[k].getContentAsWideString());
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_OR_CELL_EXPECTED);
            }
        } else if (str == L"filename") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.getContentAsWideString();
                ptrHistoryManager->setFilename(filename);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else if (str == L"load") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.getContentAsWideString();
                ptrHistoryManager->loadFromFile(filename);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else if (str == L"save") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.getContentAsWideString();
                ptrHistoryManager->saveToFile(filename);
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
            }
        } else if (str == L"duplicated") {
            nargoutcheck(nLhs, 0, 0);
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
            nargoutcheck(nLhs, 0, 0);
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
            nargoutcheck(nLhs, 0, 1);
            if (arg2.getDataClass() == NLS_DOUBLE) {
                if (arg2.isScalar()) {
                    double dvalue = arg2.getContentAsDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    if (dvalue < 0) {
                        Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
                        Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                    }
                    std::wstring line = ptrHistoryManager->get(ivalue);
                    retval << ArrayOf::characterArrayConstructor(line);
                } else {
                    Dimensions sze(arg2.getDimensions());
                    Dimensions supported(1, 2);
                    if (sze.equals(supported)) {
                        auto* dvalues = (double*)arg2.getDataPointer();
                        double dvalue1 = dvalues[0];
                        double dvalue2 = dvalues[1] + 1;
                        if (!std::isfinite(dvalue1) || !std::isfinite(dvalue2)) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (dvalue1 < 0 || dvalue2 < 0) {
                            Error(ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED);
                        }
                        auto ivalue1 = static_cast<size_t>(dvalue1);
                        auto ivalue2 = static_cast<size_t>(dvalue2);
                        if ((static_cast<double>(ivalue1) != dvalue1)
                            || (static_cast<double>(ivalue2) != dvalue2)) {
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
                            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(res);
                        }
                    } else {
                        Error(ERROR_WRONG_ARGUMENT_2_SIZE_A_B_VECTOR_EXPECTED);
                    }
                }
            } else if (str == L"saveafter") {
                nargoutcheck(nLhs, 0, 0);
                if (arg2.isScalar()) {
                    if (arg2.getDataClass() == NLS_DOUBLE) {
                        double dvalue = arg2.getContentAsDoubleScalar();
                        if (!std::isfinite(dvalue)) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        if (dvalue < 0) {
                            Error(ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED);
                        }
                        auto ivalue = static_cast<size_t>(dvalue);
                        if (static_cast<double>(ivalue) != dvalue) {
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
    auto* ptrHistoryManager
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (ptrHistoryManager == nullptr) {
        Error(_W("History manager not enabled."));
    }
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 0: {
        return historyBuiltin_no_rhs(eval, ptrHistoryManager, nLhs, argIn);
    } break;
    case 1: {
        return historyBuiltin_size_one_rhs(eval, ptrHistoryManager, nLhs, argIn);
    } break;
    case 2: {
        return historyBuiltin_two_rhs(ptrHistoryManager, nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
