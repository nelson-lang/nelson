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
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
NLSTYPES_IMPEXP std::string
ClassName(const ArrayOf& In);
NLSTYPES_IMPEXP stringVector
ClassName(const ArrayOfVector& In);
NLSTYPES_IMPEXP void
ClassName(const ArrayOf& In, std::wstring& returnedClassName);
NLSTYPES_IMPEXP void
ClassName(const ArrayOf& In, std::string& returnedClassName);
//=============================================================================
} // namespace Nelson
//=============================================================================
