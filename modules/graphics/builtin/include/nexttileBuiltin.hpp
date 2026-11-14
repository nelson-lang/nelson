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
//=============================================================================
namespace Nelson {
class GOAxis;
}
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
nexttileBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
// Place a freshly created axis object (GOAxis/GOPolarAxes/...) into the next
// empty tile of a tiled layout; takes ownership of ax. Returns the axis handle.
int64
appendAxisToTiledLayout(int64 layoutHandle, GOAxis* ax, const ArrayOfVector& properties);
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
