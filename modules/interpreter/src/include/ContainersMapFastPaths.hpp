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
#include "ArrayOf.hpp"
#include "ClassdefHandleObject.hpp"
#include "ContainersMapObject.hpp"
//=============================================================================
// Native shortcuts for a containers.Map with char keys held in native storage.
// Each returns false, leaving the call to the class methods, for anything else.
//=============================================================================
namespace Nelson {
//=============================================================================
// native storage of the containers.Map r (category checked by the caller)
ContainersMapObject*
containersMapNativeStorage(const ArrayOf& r, ClassdefHandleObject** objectOut = nullptr);
//=============================================================================
// the single {charKey} subscript of M(charKey)
bool
containersMapCharKey(const ArrayOfVector& subsindices, std::string& key);
//=============================================================================
bool
tryContainersMapCount(const ArrayOf& r, ArrayOfVector& result);
//=============================================================================
// isKey(M, key) and M.isKey(key) with a character row vector key
bool
tryContainersMapIsKey(const ArrayOf& r, const ArrayOf& key, ArrayOfVector& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
