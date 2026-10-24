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
/**
 * Upload a host array to the device and wrap it as a gpuArray handle.
 * double inputs are converted to single (f32) on upload: WebGPU/WGSL has
 * no f64 compute type. Complex and sparse inputs are rejected.
 */
NLSGPU_ENGINE_IMPEXP ArrayOf
CreateGpuArray(const ArrayOf& hostArray);
//=============================================================================
/**
 * Same as CreateGpuArray but returns the raw object (not registered as a
 * Nelson handle). Used for temporary device operands in mixed host/device
 * operations; the caller owns and deletes the object.
 */
NLSGPU_ENGINE_IMPEXP GpuArrayObject*
UploadGpuArray(const ArrayOf& hostArray);
//=============================================================================
/**
 * Download a gpuArray back to the host as a single array.
 */
NLSGPU_ENGINE_IMPEXP ArrayOf
GatherGpuArray(GpuArrayObject* gpuObject);
//=============================================================================
/**
 * Allocate an uninitialized device buffer usable as kernel input/output
 * and wrap it as a gpuArray handle. The buffer is always f32; underlyingType
 * ("single" or "logical") only affects how gather() rebuilds the host array.
 */
NLSGPU_ENGINE_IMPEXP GpuArrayObject*
AllocateGpuArray(
    const Dimensions& dims, const std::string& underlyingType = "single", bool complex = false);
//=============================================================================
/**
 * Allocate a new device array with the same dimensions and underlying type as
 * `source` and copy its buffer (device-to-device). Used for value-semantics
 * operations (indexed assignment) and reshape.
 */
NLSGPU_ENGINE_IMPEXP GpuArrayObject*
CloneGpuArray(GpuArrayObject* source);
//=============================================================================
} // namespace Nelson
//=============================================================================
