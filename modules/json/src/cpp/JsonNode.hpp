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
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
#include <simdjson.h>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class JsonNodeType
{
    Undefined,
    Logical, // true, false or a reduced logical array
    Double, // number or a reduced double array
    String, // char
    Array, // array of null values only (cell of [])
    EmptyMatrix, // null
    Struct, // object or a reduced struct array
    Cell // array that cannot be reduced
};
//=============================================================================
struct JsonComposite;
//=============================================================================
// Decoded JSON value. Scalars are stored inline, arrays and objects in a
// composite part.
struct JsonNode
{
    JsonNodeType type = JsonNodeType::Undefined;
    logical scalarLogical = false;
    double scalarDouble = std::numeric_limits<double>::quiet_NaN();
    std::string_view scalarString;
    std::unique_ptr<JsonComposite> composite;
};
//=============================================================================
struct JsonComposite
{
    // empty for an object, else the dimensions of the reduced array
    std::vector<size_t> dims;
    // array elements (Array, Cell)
    std::vector<JsonNode> elements;
    // reduced Double and Logical arrays, column-major
    std::vector<double> doubles;
    std::vector<logical> logicals;
    // Struct: field names and values of 'count' elements, fields[f * count + k]
    std::vector<std::string> fieldnames;
    std::vector<JsonNode> fields;
    size_t count = 1;
};
//=============================================================================
// State shared by one decode: valid field names of the keys already seen
// (the key views point into the parser buffer, alive during the decode).
struct JsonDecodeContext
{
    std::unordered_map<std::string_view, std::string> fieldnames;
};
//=============================================================================
// Imports a JSON value, reducing homogeneous arrays to matrices and struct
// arrays. Returns false when the value cannot be decoded.
bool
importJsonValue(simdjson::ondemand::value& value, JsonNode& node, JsonDecodeContext& context);
//=============================================================================
const std::vector<size_t>&
jsonNodeDims(const JsonNode& node);
//=============================================================================
// NaN, Inf and -Inf tokens (anything else gives NaN)
double
parseJsonSpecialNumeric(std::string_view token);
//=============================================================================
} // namespace Nelson
//=============================================================================
