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
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
//=============================================================================
#include <hdf5.h>
#include "ArrayOf.hpp"
//=============================================================================
#define NELSON_SCHEMA 1
#define NELSON_HEADER L"Nelson 1.0 NH5-file"
#define NELSON_HEADER_VERSION 0x0001
#define NELSON_HEADER_ENDIAN 0x4d49
#define NELSON_SCHEMA_STR "NELSON_schema"
#define NELSON_CLASS_STR "NELSON_class"
#define NELSON_EMPTY_STR "NELSON_empty"
#define NELSON_COMPLEX_STR "NELSON_complex"
#define NELSON_SPARSE_STR "NELSON_sparse"
#define NELSON_SPARSE_NZMAX_STR "NELSON_nzmax"
#define NELSON_DIMENSIONS_STR "NELSON_dimensions"
#define NELSON_OBJECT_STR "NELSON_object"
#define FIELDNAMES_STR "fieldnames"
#define NELSON_COMPRESSION_LEVEL 3
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isNelsonH5File(hid_t fid);
//=============================================================================
int32
getNelsonH5Schema(hid_t fid);
//=============================================================================
bool
addSchemaFormat(hid_t obj_id);
//=============================================================================
bool
updateNelsonH5Header(hid_t fid);
//=============================================================================
stringVector
getVariableNames(hid_t fid);
//=============================================================================
std::string
getNelsonClass(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
Dimensions
getNelsonDimensions(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonEmpty(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonSparse(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonObject(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonComplex(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
uint64
getNelsonNzmax(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
hid_t
setCompression(const Dimensions& dims, bool useCompression);
//=============================================================================
} // namespace Nelson
//=============================================================================
