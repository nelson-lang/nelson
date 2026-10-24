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
#include "HandleGenericObject.hpp"
#include "nlsInterpreter_exports.h"
#include <string>
#include <unordered_map>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP ContainersMapObject : public HandleGenericObject
{
private:
    std::vector<std::string> _keys;
    std::vector<ArrayOf> _values;
    std::vector<double> _doubleValues;
    mutable std::vector<ArrayOf> _doubleArrayValuesCache;
    std::unordered_map<std::string, size_t> _positions;
    bool _hasDoubleValues = false;
    // Non-char (numeric) key maps keep the original scalar key next to its
    // canonical string encoding, so keys() hands back keys of the original type.
    std::vector<ArrayOf> _keyArrays;
    bool _hasArrayKeys = false;

    ArrayOf
    doubleValueAsArray(size_t index) const;

public:
    ContainersMapObject(std::vector<std::string>&& keys, std::vector<ArrayOf>&& values);
    ContainersMapObject(std::vector<std::string>&& keys, std::vector<ArrayOf>&& values,
        std::unordered_map<std::string, size_t>&& positions);
    ContainersMapObject(std::vector<std::string>&& keys, std::vector<double>&& values);
    ContainersMapObject(std::vector<std::string>&& keys, std::vector<double>&& values,
        std::unordered_map<std::string, size_t>&& positions);

    bool
    lookup(const std::string& key, ArrayOf& value) const;

    void
    set(const std::string& key, const ArrayOf& value);

    // Set under a canonical string encoding while remembering the original
    // (numeric) key, so keys() can return it with its original type.
    void
    setWithOriginalKey(const std::string& key, const ArrayOf& originalKey, const ArrayOf& value);

    bool
    removeKey(const std::string& key);

    bool
    isKey(const std::string& key) const;

    size_t
    count() const;

    ArrayOf
    keys() const;

    ArrayOf
    values() const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
