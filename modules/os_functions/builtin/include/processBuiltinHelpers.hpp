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
#include "HandleManager.hpp"
#include "ProcessObject.hpp"
//=============================================================================
namespace Nelson::OsFunctionsGateway {
//=============================================================================
std::vector<HandleRef>
getProcessHandleRefs(const ArrayOf& value);
//=============================================================================
std::vector<ProcessObject*>
getProcessObjects(const ArrayOf& value);
//=============================================================================
ProcessObject*
getProcessObjectScalar(const ArrayOf& value);
//=============================================================================
} // namespace Nelson::OsFunctionsGateway
//=============================================================================
