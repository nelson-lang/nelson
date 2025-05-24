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
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveClassAttribute(hid_t fid, const std::string& location, const ArrayOf& variableValue);
//=============================================================================
bool
h5SaveDimensionsAttribute(hid_t fid, const std::string& location, const Dimensions& dims);
//=============================================================================
bool
h5SaveEmptyAttribute(hid_t fid, const std::string& location, bool isEmpty = true);
//=============================================================================
bool
h5SaveComplexAttribute(hid_t fid, const std::string& location, bool isComplex = true);
//=============================================================================
bool
h5SaveSparseAttribute(hid_t fid, const std::string& location, bool isSparse = true);
//=============================================================================
bool
h5SaveStringAttribute(hid_t fid, const std::string& location, const std::string& attributeName,
    const std::string& content);
//=============================================================================
bool
h5SaveUint8Attribute(
    hid_t fid, const std::string& location, const std::string& attributeName, uint8 content);
//=============================================================================
bool
h5SaveUint64Attribute(
    hid_t fid, const std::string& location, const std::string& attributeName, uint64 content);
//=============================================================================
bool
h5LDeleteIfExists(hid_t fid, const std::string& location);
//=============================================================================
} // namespace Nelson
//=============================================================================
