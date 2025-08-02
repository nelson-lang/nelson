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
#include "ArrayOf.hpp"
#include "QObjectHandleObject.hpp"
#include "nlsQml_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSQML_ENGINE_IMPEXP void
fieldnamesQObjectHandleObject(const ArrayOf& A, bool fullList, wstringVector& fieldnames);
NLSQML_ENGINE_IMPEXP void
fieldnamesQObjectHandleObject(
    QObjectHandleObject* qmlHandle, bool fullList, wstringVector& fieldnames);
//=============================================================================
} // namespace Nelson
//=============================================================================
