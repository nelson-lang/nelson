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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "HistoryManager.hpp"
#include "NelsonConfiguration.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        std::wstring str = arg1.asWideString();
        if (str == L"size") {
            retval << ArrayOf::doubleScalar(
                static_cast<double>(ptrHistoryManager->getLastNCommandsSize()));
        } else if (str == L"enable_save") {
            retval << ArrayOf::logicalScalar(ptrHistoryManager->getSaveEnabled());
        } else if (str == L"filename") {
            retval << ArrayOf::characterArray(ptrHistoryManager->getFilename());
        } else if (str == L"get") {
            wstringVector res = ptrHistoryManager->get();
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(res);
        } else if (str == L"display") {
            if (nLhs == 0) {
                wstringVector lines = ptrHistoryManager->get();
                if (!lines.empty()) {
                    Interface* io = eval->getInterface();
                    if (io) {
                        for (size_t k = 0; (k < lines.size())
                            && !NelsonConfiguration::getInstance()->getInterruptPending();
                            k++) {
                            io->outputMessage(std::to_wstring(k) + L" : " + lines[k] + L"\n");
                        }
                    }
                }
            } else {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            }
        } else if (str == L"save") {
            if (nLhs == 0) {
                ptrHistoryManager->saveToFile();
            } else {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            }
        } else if (str == L"load") {
            if (nLhs == 0) {
                ptrHistoryManager->loadFromFile();
            } else {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            }
        } else if (str == L"clear") {
            if (nLhs == 0) {
                ptrHistoryManager->clear(true);
            } else {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            }
        } else if (str == L"duplicated") {
            retval << ArrayOf::logicalScalar(ptrHistoryManager->getAllowDuplicatedLines());
        } else if (str == L"saveafter") {
            retval << ArrayOf::doubleScalar(
                static_cast<double>(ptrHistoryManager->getSaveAfterNCommands()));
        } else if (str == L"removeexit") {
            retval << ArrayOf::logicalScalar(ptrHistoryManager->getRemoveExit());
        } else {
            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
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
            for (size_t k = 0;
                k < res.size() && !NelsonConfiguration::getInstance()->getInterruptPending(); k++) {
                io->outputMessage(std::to_wstring(k + 1) + L" : " + res[k] + L"\n");
            }
        }
    } else {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(res);
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
        std::wstring str = arg1.asWideString();
        if (str == L"size") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isScalar()) {
                if (arg2.dataClass() == NLS_DOUBLE) {
                    double dvalue = arg2.asDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #2");
                    }
                    if (dvalue < 0) {
                        Error(L"Nelson:expectedNonnegative", L"%s must be nonnegative.",
                            L"Argument #2");
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
                        Error(L"Nelson:expectedInteger", L"%s must be an integer.", L"Argument #2");
                    }
                    ptrHistoryManager->setLastNCommandsSize(ivalue);
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                        L"double");
                }
            } else {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (2));
            }
        } else if (str == L"enable_save") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isScalar()) {
                if (arg2.isLogical()) {
                    logical bEnable = arg2.asLogicalScalar();
                    ptrHistoryManager->setSaveEnabled(bEnable ? true : false);
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                        L"logical");
                }
            } else {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (2));
            }
        } else if (str == L"delete") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.dataClass() == NLS_DOUBLE) {
                if (arg2.isScalar()) {
                    double dvalue = arg2.asDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #2");
                    }
                    if (dvalue < 0) {
                        Error(L"Nelson:expectedNonnegative", L"%s must be nonnegative.",
                            L"Argument #2");
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
                        Error(L"Nelson:expectedInteger", L"%s must be an integer.", L"Argument #2");
                    }
                    ptrHistoryManager->remove(ivalue);
                } else {
                    Dimensions sze(arg2.dimensions());
                    Dimensions supported(1, 2);
                    if (sze.equals(supported)) {
                        auto* dvalues = (double*)arg2.dataPointer();
                        double dvalue1 = dvalues[0];
                        double dvalue2 = dvalues[1] + 1;
                        if (!std::isfinite(dvalue1) || !std::isfinite(dvalue2)) {
                            Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #2");
                        }
                        if (dvalue1 < 0 || dvalue2 < 0) {
                            Error(L"Nelson:expectedNonnegative", L"%s must be nonnegative.",
                                L"Argument #2");
                        }
                        auto ivalue1 = static_cast<size_t>(dvalue1);
                        auto ivalue2 = static_cast<size_t>(dvalue2);
                        if ((static_cast<double>(ivalue1) != dvalue1)
                            || (static_cast<double>(ivalue2) != dvalue2)) {
                            Error(L"Nelson:expectedInteger", L"%s must be an integer.",
                                L"Argument #2");
                        }
                        if (ivalue2 <= ivalue1) {
                            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                        }
                        if ((ivalue1 >= ptrHistoryManager->getCurrentSize())
                            || (ivalue2 >= ptrHistoryManager->getCurrentSize())) {
                            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                        } else {
                            ptrHistoryManager->remove(ivalue1, ivalue2);
                        }
                    } else {
                        Error(L"Nelson:history:invalidRange", L"[a, b] range expected.");
                    }
                }
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"double");
            }
        } else if (str == L"append") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                ptrHistoryManager->appendLine(arg2.asWideString());
            } else if (arg2.isCellArrayOfCharacterVectors()) {
                ArrayOf cell(arg2);
                auto* arg = (ArrayOf*)(cell.dataPointer());
                indexType elementCount = arg2.elementCount();
                for (indexType k = 0; k < elementCount; k++) {
                    ptrHistoryManager->appendLine(arg[k].asWideString());
                }
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"string or cell");
            }
        } else if (str == L"filename") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.asWideString();
                ptrHistoryManager->setFilename(filename);
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"string");
            }
        } else if (str == L"load") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.asWideString();
                ptrHistoryManager->loadFromFile(filename);
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"string");
            }
        } else if (str == L"save") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isRowVectorCharacterArray()) {
                std::wstring filename = arg2.asWideString();
                ptrHistoryManager->saveToFile(filename);
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"string");
            }
        } else if (str == L"duplicated") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isScalar()) {
                if (arg2.isLogical()) {
                    logical bDuplicated = arg2.asLogicalScalar();
                    ptrHistoryManager->setAllowDuplicatedLines(bDuplicated ? true : false);
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                        L"logical");
                }
            } else {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (2));
            }
        } else if (str == L"removeexit") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isScalar()) {
                if (arg2.isLogical()) {
                    logical bRemove = arg2.asLogicalScalar();
                    ptrHistoryManager->setRemoveExit(bRemove ? true : false);
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                        L"logical");
                }
            } else {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (2));
            }
        } else if (str == L"get") {
            nargoutcheck(nLhs, 0, 1);
            if (arg2.dataClass() == NLS_DOUBLE) {
                if (arg2.isScalar()) {
                    double dvalue = arg2.asDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #2");
                    }
                    if (dvalue < 0) {
                        Error(L"Nelson:expectedNonnegative", L"%s must be nonnegative.",
                            L"Argument #2");
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
                        Error(L"Nelson:expectedInteger", L"%s must be an integer.", L"Argument #2");
                    }
                    std::wstring line = ptrHistoryManager->get(ivalue);
                    retval << ArrayOf::characterArray(line);
                } else {
                    Dimensions sze(arg2.dimensions());
                    Dimensions supported(1, 2);
                    if (sze.equals(supported)) {
                        auto* dvalues = (double*)arg2.dataPointer();
                        double dvalue1 = dvalues[0];
                        double dvalue2 = dvalues[1] + 1;
                        if (!std::isfinite(dvalue1) || !std::isfinite(dvalue2)) {
                            Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #2");
                        }
                        if (dvalue1 < 0 || dvalue2 < 0) {
                            Error(L"Nelson:expectedNonnegative", L"%s must be nonnegative.",
                                L"Argument #2");
                        }
                        auto ivalue1 = static_cast<size_t>(dvalue1);
                        auto ivalue2 = static_cast<size_t>(dvalue2);
                        if ((static_cast<double>(ivalue1) != dvalue1)
                            || (static_cast<double>(ivalue2) != dvalue2)) {
                            Error(L"Nelson:expectedInteger", L"%s must be an integer.",
                                L"Argument #2");
                        }
                        if (ivalue2 <= ivalue1) {
                            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                        }
                        if ((ivalue1 >= ptrHistoryManager->getCurrentSize())
                            || (ivalue2 >= ptrHistoryManager->getCurrentSize())) {
                            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
                        } else {
                            wstringVector res = ptrHistoryManager->get(ivalue1, ivalue2);
                            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(res);
                        }
                    } else {
                        Error(L"Nelson:history:invalidRange", L"[a, b] range expected.");
                    }
                }
            } else {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                    L"double");
            }
        } else if (str == L"saveafter") {
            nargoutcheck(nLhs, 0, 0);
            if (arg2.isScalar()) {
                if (arg2.dataClass() == NLS_DOUBLE) {
                    double dvalue = arg2.asDoubleScalar();
                    if (!std::isfinite(dvalue)) {
                        Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Argument #2");
                    }
                    if (dvalue < 0) {
                        Error(L"Nelson:expectedNonnegative", L"%s must be nonnegative.",
                            L"Argument #2");
                    }
                    auto ivalue = static_cast<size_t>(dvalue);
                    if (static_cast<double>(ivalue) != dvalue) {
                        Error(L"Nelson:expectedInteger", L"%s must be an integer.", L"Argument #2");
                    }
                    ptrHistoryManager->setSaveAfterNCommands(ivalue);
                } else {
                    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                        L"double");
                }
            } else {
                Error(L"Nelson:scalarExpected", L"Wrong size for argument #%d: scalar expected.",
                    (2));
            }
        } else {
            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (1));
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
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
        Error(L"Nelson:history:notEnabled", L"History manager not enabled.");
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
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
