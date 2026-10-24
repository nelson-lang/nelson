//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
Evaluator::setLastErrorException(const NelsonException& e)
{
    NelsonException* ptrPreviousException
        = (NelsonException*)NelsonConfiguration::getInstance()->getLastErrorException(getID());
    if (ptrPreviousException) {
        delete ptrPreviousException;
    }
    if (e.isEmpty()) {
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
        return true;
    }
    try {
        NelsonException* ptrException = new NelsonException(e);
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), ptrException);
    } catch (const std::bad_alloc&) {
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
        return false;
    }
    return true;
}
//=============================================================================
NelsonException
Evaluator::getLastErrorException()
{
    NelsonException* ptrException = static_cast<NelsonException*>(
        NelsonConfiguration::getInstance()->getLastErrorException(getID()));
    NelsonException lastException;
    if (ptrException) {
        lastException = *ptrException;
    }
    return lastException;
}
//=============================================================================
void
Evaluator::resetLastErrorException()
{
    NelsonException* ptrException
        = (NelsonException*)NelsonConfiguration::getInstance()->getLastErrorException(getID());
    if (ptrException) {
        delete ptrException;
    }
    NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
}
//=============================================================================
NelsonException
Evaluator::getLastUncaughtException()
{
    return lastUncaughtException;
}
//=============================================================================
bool
Evaluator::setLastUncaughtException(const NelsonException& e)
{
    lastUncaughtException = e;
    return true;
}
//=============================================================================
void
Evaluator::resetLastUncaughtException()
{
    lastUncaughtException.clear();
}
//=============================================================================
NelsonException
Evaluator::getLastWarningException()
{
    NelsonException* ptrException = static_cast<NelsonException*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    NelsonException lastException;
    if (ptrException) {
        lastException = *ptrException;
    }
    return lastException;
}
//=============================================================================
void
Evaluator::resetLastWarningException()
{
    NelsonException* ptrException = static_cast<NelsonException*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    if (ptrException) {
        delete ptrException;
    }
    NelsonConfiguration::getInstance()->setLastWarningException(getID(), nullptr);
}
//=============================================================================
bool
Evaluator::setLastWarningException(const NelsonException& e)
{
    NelsonException* ptrPreviousException = static_cast<NelsonException*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    if (ptrPreviousException) {
        delete ptrPreviousException;
    }
    if (e.isEmpty()) {
        NelsonConfiguration::getInstance()->setLastWarningException(getID(), nullptr);
        return true;
    }
    try {
        NelsonException* ptrException = new NelsonException(e);
        NelsonConfiguration::getInstance()->setLastWarningException(getID(), ptrException);
    } catch (const std::bad_alloc&) {
        NelsonConfiguration::getInstance()->setLastWarningException(getID(), nullptr);
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
