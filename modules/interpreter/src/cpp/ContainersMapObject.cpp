//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ContainersMapObject.hpp"
//=============================================================================
// no dot: a dotted category is probed as a package class on every builtin
// call taking the storage (isempty, class, ...), one file stat per path entry
#define CONTAINERS_MAP_NATIVE_STORAGE_CATEGORY "__containers_map_native_storage__"
//=============================================================================
namespace Nelson {
//=============================================================================
ContainersMapObject::ContainersMapObject(
    std::vector<std::string>&& keys, std::vector<ArrayOf>&& values)
    : ContainersMapObject(
          std::move(keys), std::move(values), std::unordered_map<std::string, size_t> {})
{
    _positions.reserve(_keys.size());
    for (size_t k = 0; k < _keys.size(); ++k) {
        _positions.emplace(_keys[k], k);
    }
}
//=============================================================================
ContainersMapObject::ContainersMapObject(std::vector<std::string>&& keys,
    std::vector<ArrayOf>&& values, std::unordered_map<std::string, size_t>&& positions)
    : HandleGenericObject(CONTAINERS_MAP_NATIVE_STORAGE_CATEGORY, this, false)
    , _keys(std::move(keys))
    , _values(std::move(values))
    , _positions(std::move(positions))
{
}
//=============================================================================
ContainersMapObject::ContainersMapObject(
    std::vector<std::string>&& keys, std::vector<double>&& values)
    : ContainersMapObject(
          std::move(keys), std::move(values), std::unordered_map<std::string, size_t> {})
{
    _positions.reserve(_keys.size());
    for (size_t k = 0; k < _keys.size(); ++k) {
        _positions.emplace(_keys[k], k);
    }
}
//=============================================================================
ContainersMapObject::ContainersMapObject(std::vector<std::string>&& keys,
    std::vector<double>&& values, std::unordered_map<std::string, size_t>&& positions)
    : HandleGenericObject(CONTAINERS_MAP_NATIVE_STORAGE_CATEGORY, this, false)
    , _keys(std::move(keys))
    , _doubleValues(std::move(values))
    , _positions(std::move(positions))
    , _hasDoubleValues(true)
{
}
//=============================================================================
ArrayOf
ContainersMapObject::doubleValueAsArray(size_t index) const
{
    if (_doubleArrayValuesCache.empty() && !_doubleValues.empty()) {
        _doubleArrayValuesCache.resize(_doubleValues.size());
    }
    ArrayOf& cachedValue = _doubleArrayValuesCache[index];
    if (cachedValue.isEmpty()) {
        cachedValue = ArrayOf::doubleScalar(_doubleValues[index]);
    }
    return cachedValue;
}
//=============================================================================
bool
ContainersMapObject::lookup(const std::string& key, ArrayOf& value) const
{
    auto found = _positions.find(key);
    if (found == _positions.end()) {
        return false;
    }
    if (_hasDoubleValues) {
        value = doubleValueAsArray(found->second);
        return true;
    }
    value = _values[found->second];
    return true;
}
//=============================================================================
void
ContainersMapObject::setWithOriginalKey(
    const std::string& key, const ArrayOf& originalKey, const ArrayOf& value)
{
    _hasArrayKeys = true;
    auto found = _positions.find(key);
    if (found != _positions.end()) {
        _values[found->second] = value;
        return;
    }
    _positions.emplace(key, _keys.size());
    _keys.push_back(key);
    _keyArrays.push_back(originalKey);
    _values.push_back(value);
}
//=============================================================================
void
ContainersMapObject::set(const std::string& key, const ArrayOf& value)
{
    const bool valueIsDoubleScalar
        = value.isDoubleType(true) && !value.isSparse() && value.isScalar();
    if (_hasDoubleValues && !valueIsDoubleScalar) {
        // promote the dense double storage so arbitrary values can be held
        _values.reserve(_doubleValues.size());
        for (size_t k = 0; k < _doubleValues.size(); ++k) {
            _values.push_back(doubleValueAsArray(k));
        }
        _doubleValues.clear();
        _doubleArrayValuesCache.clear();
        _hasDoubleValues = false;
    }
    auto found = _positions.find(key);
    if (found != _positions.end()) {
        if (_hasDoubleValues) {
            _doubleValues[found->second] = value.asDoubleScalar();
            if (found->second < _doubleArrayValuesCache.size()) {
                _doubleArrayValuesCache[found->second] = ArrayOf();
            }
        } else {
            _values[found->second] = value;
        }
        return;
    }
    _positions.emplace(key, _keys.size());
    _keys.push_back(key);
    if (_hasDoubleValues) {
        _doubleValues.push_back(value.asDoubleScalar());
        if (!_doubleArrayValuesCache.empty()) {
            _doubleArrayValuesCache.push_back(ArrayOf());
        }
    } else {
        _values.push_back(value);
    }
}
//=============================================================================
bool
ContainersMapObject::removeKey(const std::string& key)
{
    auto found = _positions.find(key);
    if (found == _positions.end()) {
        return false;
    }
    const size_t index = found->second;
    _keys.erase(_keys.begin() + index);
    if (_hasArrayKeys) {
        _keyArrays.erase(_keyArrays.begin() + index);
    }
    if (_hasDoubleValues) {
        _doubleValues.erase(_doubleValues.begin() + index);
        _doubleArrayValuesCache.clear();
    } else {
        _values.erase(_values.begin() + index);
    }
    _positions.erase(found);
    for (auto& position : _positions) {
        if (position.second > index) {
            --position.second;
        }
    }
    return true;
}
//=============================================================================
bool
ContainersMapObject::isKey(const std::string& key) const
{
    return _positions.find(key) != _positions.end();
}
//=============================================================================
size_t
ContainersMapObject::count() const
{
    return _keys.size();
}
//=============================================================================
ArrayOf
ContainersMapObject::keys() const
{
    const size_t keyCount = _keys.size();
    Dimensions dims(1, static_cast<indexType>(keyCount));
    if (keyCount == 0) {
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, keyCount, stringVector(), false));
    if (_hasArrayKeys) {
        // numeric-keyed map: hand back the original scalar keys of their type
        for (size_t k = 0; k < keyCount; ++k) {
            elements[k] = _keyArrays[k];
        }
    } else {
        for (size_t k = 0; k < keyCount; ++k) {
            elements[k] = ArrayOf::characterArray(_keys[k]);
        }
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
ArrayOf
ContainersMapObject::values() const
{
    const size_t valueCount = _hasDoubleValues ? _doubleValues.size() : _values.size();
    Dimensions dims(1, static_cast<indexType>(valueCount));
    if (valueCount == 0) {
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, valueCount, stringVector(), false));
    if (_hasDoubleValues) {
        for (size_t k = 0; k < valueCount; ++k) {
            elements[k] = doubleValueAsArray(k);
        }
    } else {
        for (size_t k = 0; k < valueCount; ++k) {
            elements[k] = _values[k];
        }
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
