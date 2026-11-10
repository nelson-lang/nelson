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
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Bessel function kind 0..5 (J, Y, I, K, H1, H2) of real NU and Z on the device;
// false sends the call to the host (complex result, large order or argument, other sizes)
NLSGPU_ENGINE_IMPEXP bool
GpuBessel(int kind, GpuArrayObject* nu, GpuArrayObject* z, bool scaled, ArrayOf& result);
//=============================================================================
// Airy function k = 0..3 of real Z on the device; false sends the call to the host
NLSGPU_ENGINE_IMPEXP bool
GpuAiry(int k, GpuArrayObject* z, bool scaled, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
