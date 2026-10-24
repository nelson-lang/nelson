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
#include <string>
#include "Types.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
using GraphicsObjectClassNameResolver = std::string (*)(go_handle handle);
//=============================================================================
NLSTYPES_IMPEXP void
setGraphicsObjectClassNameResolver(GraphicsObjectClassNameResolver resolver);
//=============================================================================
NLSTYPES_IMPEXP std::string
resolveGraphicsObjectClassName(go_handle handle);
//=============================================================================
} // namespace Nelson
//=============================================================================
