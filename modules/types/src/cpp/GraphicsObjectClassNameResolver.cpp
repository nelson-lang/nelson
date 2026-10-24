//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsObjectClassNameResolver.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GraphicsObjectClassNameResolver graphicsObjectClassNameResolver = nullptr;
//=============================================================================
void
setGraphicsObjectClassNameResolver(GraphicsObjectClassNameResolver resolver)
{
    graphicsObjectClassNameResolver = resolver;
}
//=============================================================================
std::string
resolveGraphicsObjectClassName(go_handle handle)
{
    if (graphicsObjectClassNameResolver == nullptr) {
        return {};
    }
    return graphicsObjectClassNameResolver(handle);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
