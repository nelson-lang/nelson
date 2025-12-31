//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JsonVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
JsonVariable::JsonVariable()
    : jsonVariableType(JSON_TO_NELSON_UNDEFINED)
    , scalarDouble(std::nan(""))
    , scalarLogical(false)
    , reduced(false)
{
    // Initialize dims to empty vector
    dims.clear();
    // Initialize scalarString to empty string
    scalarString = {};
    // Initialize scalarMap to empty map
    scalarMap.clear();
    // Initialize vectorString, vectorDouble, vectorLogical to empty vectors
    vectorString.clear();
    vectorDouble.clear();
    vectorLogical.clear();
    // Initialize vectorJsonVariable to empty vector
    vectorJsonVariable.clear();
    // Initialize map to empty unordered_map
    map.clear();
}
//=============================================================================
JsonVariable::~JsonVariable() { clear(); };
//=============================================================================
JsonVariable::JsonVariable(const JsonVariable& other) { deepCopyFrom(other); }
//=============================================================================
void
JsonVariable::clear()
{
    for (auto& kv : scalarMap) {
        delete kv.second;
    }
    scalarMap.clear();
    for (auto ptr : vectorJsonVariable) {
        delete ptr;
    }
    vectorJsonVariable.clear();
    for (auto& kv : map) {
        for (auto ptr : kv.second) {
            delete ptr;
        }
        kv.second.clear();
    }
    map.clear();
}
//=============================================================================
JsonVariable&
JsonVariable::operator=(const JsonVariable& other)
{
    if (this != &other) {
        clear();
        deepCopyFrom(other);
    }
    return *this;
}
//=============================================================================
void
JsonVariable::deepCopyFrom(const JsonVariable& other)
{
    jsonVariableType = other.jsonVariableType;
    dims = other.dims;
    scalarString = other.scalarString;
    scalarDouble = other.scalarDouble;
    scalarLogical = other.scalarLogical;

    // Deep copy for scalarMap
    for (const auto& kv : other.scalarMap) {
        scalarMap[kv.first] = kv.second ? new JsonVariable(*kv.second) : nullptr;
    }

    vectorString = other.vectorString;
    vectorDouble = other.vectorDouble;
    vectorLogical = other.vectorLogical;

    // Deep copy for vectorJsonVariable
    for (const auto ptr : other.vectorJsonVariable) {
        vectorJsonVariable.push_back(ptr ? new JsonVariable(*ptr) : nullptr);
    }

    // Deep copy for map
    for (const auto& kv : other.map) {
        std::vector<JsonVariable*> vec;
        for (const auto ptr : kv.second) {
            vec.push_back(ptr ? new JsonVariable(*ptr) : nullptr);
        }
        map[kv.first] = std::move(vec);
    }

    fieldnames = other.fieldnames;
    reduced = other.reduced;
}
//=============================================================================
}
//=============================================================================
