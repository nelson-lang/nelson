//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timerBuiltin.hpp"
#include "DisplayVariableHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "TimerHelpers.hpp"
#include "TimerObject.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
displayTimer(Evaluator* eval, const ArrayOf& value, const std::wstring& name)
{
    Interface* io = eval->getInterface();
    DisplayVariableHeader(io, value, name, false);
    if (value.isScalar()) {
        std::vector<TimerObject*> objects = getTimerObjects(value);
        if (!objects.empty()) {
            objects[0]->disp(io);
        }
    }
    DisplayVariableFooter(io, name.empty());
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_dispBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isHandle() || argIn[0].handleCategory() != NLS_HANDLE_TIMER_CATEGORY_STR) {
        Error(L"Nelson:time:timerHandleExpected", L"%s expected.", L"timer handle");
    }
    displayTimer(eval, argIn[0], L"");
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_displayBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isHandle() || argIn[0].handleCategory() != NLS_HANDLE_TIMER_CATEGORY_STR) {
        Error(L"Nelson:time:timerHandleExpected", L"%s expected.", L"timer handle");
    }
    std::wstring name = argIn[0].wname();
    if (argIn.size() == 2) {
        name = argIn[1].asWideString();
    }
    displayTimer(eval, argIn[0], name);
    return {};
}
//=============================================================================
