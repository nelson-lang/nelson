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
Evaluator::setLastErrorException(const Exception& e)
{
    Exception* ptrPreviousException
        = (Exception*)NelsonConfiguration::getInstance()->getLastErrorException(getID());
    if (ptrPreviousException) {
        delete ptrPreviousException;
    }
    try {
        Exception* ptrException = new Exception(e);
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), ptrException);
    } catch (const std::bad_alloc&) {
        NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
        return false;
    }
    return true;
}
//=============================================================================
Exception
Evaluator::getLastErrorException()
{
    Exception* ptrException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastErrorException(getID()));
    Exception lastException;
    if (ptrException) {
        lastException = *ptrException;
    }
    return lastException;
}
//=============================================================================
void
Evaluator::resetLastErrorException()
{
    Exception* ptrException
        = (Exception*)NelsonConfiguration::getInstance()->getLastErrorException(getID());
    if (ptrException) {
        delete ptrException;
    }
    NelsonConfiguration::getInstance()->setLastErrorException(getID(), nullptr);
}
//=============================================================================
Exception
Evaluator::getLastWarningException()
{
    Exception* ptrException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    Exception lastException;
    if (ptrException) {
        lastException = *ptrException;
    }
    return lastException;
}
//=============================================================================
void
Evaluator::resetLastWarningException()
{
    Exception* ptrException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    if (ptrException) {
        delete ptrException;
    }
    NelsonConfiguration::getInstance()->setLastWarningException(getID(), nullptr);
}
//=============================================================================
bool
Evaluator::setLastWarningException(const Exception& e)
{
    Exception* ptrPreviousException = static_cast<Exception*>(
        NelsonConfiguration::getInstance()->getLastWarningException(getID()));
    if (ptrPreviousException) {
        delete ptrPreviousException;
    }
    try {
        Exception* ptrException = new Exception(e);
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
