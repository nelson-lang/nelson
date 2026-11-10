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
#include <vector>
#include "ArrayOf.hpp"
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// run the element-wise function `name` on the device; false when the host must handle it
// (complex or integer input, or a value whose host result is an error)
NLSGPU_ENGINE_IMPEXP bool
GpuElementwiseMath(const std::string& name, const ArrayOfVector& args, ArrayOf& result);
//=============================================================================
// same, for operands that are already device arrays
NLSGPU_ENGINE_IMPEXP bool
GpuElementwiseMath(
    const std::string& name, const std::vector<GpuArrayObject*>& operands, ArrayOf& result);
//=============================================================================
// regularized incomplete beta of x, a, b (one size or scalars); false sends it to the host
NLSGPU_ENGINE_IMPEXP bool
GpuBetainc(GpuArrayObject* x, GpuArrayObject* a, GpuArrayObject* b, bool upper, ArrayOf& result);
//=============================================================================
// WGSL library of accurate single precision functions (nlsSin, nlsAtan2, nlsCsqrt...)
NLSGPU_ENGINE_IMPEXP const std::string&
GpuMathHelpersWgsl();
//=============================================================================
} // namespace Nelson
//=============================================================================
