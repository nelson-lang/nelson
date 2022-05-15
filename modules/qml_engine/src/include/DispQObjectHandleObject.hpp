//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "Interface.hpp"
#include "QObjectHandleObject.hpp"
#include "nlsQml_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSQML_ENGINE_IMPEXP void
DispQObjectHandleObject(Interface* io, const ArrayOf& A, const std::string& name);
//=============================================================================
} // namespace Nelson
//=============================================================================
