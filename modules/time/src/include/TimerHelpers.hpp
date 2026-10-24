//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include "ArrayOf.hpp"
#include "HandleState.hpp"
#include "TimerObject.hpp"
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTIME_IMPEXP std::vector<TimerObject*>
getTimerObjects(const ArrayOf& value);
//=============================================================================
NLSTIME_IMPEXP std::vector<HandleRef>
getTimerHandleRefs(const ArrayOf& value);
//=============================================================================
NLSTIME_IMPEXP ArrayOf
timerHandleRefsToArray(const std::vector<HandleRef>& refs);
//=============================================================================
NLSTIME_IMPEXP ArrayOf
timerObjectsToArray(const std::vector<TimerObject*>& objects);
//=============================================================================
} // namespace Nelson
//=============================================================================
