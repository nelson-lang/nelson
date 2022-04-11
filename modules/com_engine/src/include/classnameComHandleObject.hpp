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
#include "ArrayOf.hpp"
#include "ComHandleObject.hpp"
#include "nlsCom_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSCOM_ENGINE_IMPEXP void
classnameComHandle(const ArrayOf& A, std::wstring& classname);
NLSCOM_ENGINE_IMPEXP void
classnameComHandle(ComHandleObject* comHandle, std::wstring& classname);
NLSCOM_ENGINE_IMPEXP void
classnameComHandle(const ArrayOf& A, wstringVector& classname);
//=============================================================================
} // namespace Nelson
//=============================================================================
