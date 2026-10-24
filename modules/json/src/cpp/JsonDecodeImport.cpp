//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <unordered_set>
#include "JsonNode.hpp"
#include "MakeValidFieldname.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace simdjson;
//=============================================================================
static bool
importArray(ondemand::array& array, JsonNode& node, JsonDecodeContext& context);
//=============================================================================
static bool
importObject(ondemand::object& object, JsonNode& node, JsonDecodeContext& context);
//=============================================================================
static bool
importScalar(ondemand::value& value, JsonNode& node);
//=============================================================================
static bool
importScalarByTrial(ondemand::value& value, JsonNode& node);
//=============================================================================
static JsonNodeType
commonElementType(const std::vector<JsonNode>& elements);
//=============================================================================
static void
reduceNumericArray(JsonNode& node, JsonNodeType type);
//=============================================================================
static void
reduceStructArray(JsonNode& node);
//=============================================================================
static const std::string&
validFieldname(std::string_view key, JsonDecodeContext& context);
//=============================================================================
const std::vector<size_t>&
jsonNodeDims(const JsonNode& node)
{
    static const std::vector<size_t> scalarDims;
    return node.composite ? node.composite->dims : scalarDims;
}
//=============================================================================
bool
importJsonValue(ondemand::value& value, JsonNode& node, JsonDecodeContext& context)
{
    {
        auto array_result = value.get_array();
        if (!array_result.error()) {
            auto array = array_result.value();
            return importArray(array, node, context);
        }
    }
    {
        auto object_result = value.get_object();
        if (!object_result.error()) {
            auto object = object_result.value();
            return importObject(object, node, context);
        }
    }
    return importScalar(value, node);
}
//=============================================================================
static bool
importArray(ondemand::array& array, JsonNode& node, JsonDecodeContext& context)
{
    node.type = JsonNodeType::Array;
    node.composite = std::make_unique<JsonComposite>();
    std::vector<JsonNode>& elements = node.composite->elements;
    // counting first also validates the array: malformed input fails here
    size_t count = array.count_elements();
    elements.reserve(count);
    for (auto element : array) {
        // value() throws on a malformed element: the caller reports it
        auto elem_value_result = element.value();
        if (element.error()) {
            return false;
        }
        auto elem_value = element.value();
        elements.emplace_back();
        if (!importJsonValue(elem_value, elements.back(), context)) {
            return false;
        }
    }
    JsonNodeType commonType = commonElementType(elements);
    if (commonType == JsonNodeType::Undefined) {
        node.type = JsonNodeType::Cell;
        return true;
    }
    // same dimensions for every element, else a cell
    const std::vector<size_t> refDims = jsonNodeDims(elements[0]);
    for (const JsonNode& element : elements) {
        if (jsonNodeDims(element) != refDims) {
            node.type = JsonNodeType::Cell;
            return true;
        }
    }
    std::vector<size_t>& dims = node.composite->dims;
    if (refDims.empty()) {
        dims.push_back(elements.size());
    } else if (refDims.size() == 1) {
        dims = { elements.size(), refDims[0] };
    } else {
        dims = refDims;
        dims.push_back(elements.size());
    }
    switch (commonType) {
    case JsonNodeType::Double:
    case JsonNodeType::Logical: {
        reduceNumericArray(node, commonType);
    } break;
    case JsonNodeType::Struct: {
        reduceStructArray(node);
    } break;
    default: {
        // null values only: kept as a cell of []
    } break;
    }
    return true;
}
//=============================================================================
static bool
importObject(ondemand::object& object, JsonNode& node, JsonDecodeContext& context)
{
    node.type = JsonNodeType::Struct;
    node.composite = std::make_unique<JsonComposite>();
    std::vector<std::string>& fieldnames = node.composite->fieldnames;
    std::vector<JsonNode>& fields = node.composite->fields;
    size_t field_count = object.count_fields();
    fieldnames.reserve(field_count);
    fields.reserve(field_count);
    // names already used, for the duplicate key suffixes: a set only for
    // large objects (an empty set allocates)
    std::unique_ptr<std::unordered_set<std::string>> used;
    const size_t linearSearchLimit = 32;
    auto isUsed = [&](const std::string& name) {
        if (!used && fieldnames.size() < linearSearchLimit) {
            return std::find(fieldnames.begin(), fieldnames.end(), name) != fieldnames.end();
        }
        if (!used) {
            used = std::make_unique<std::unordered_set<std::string>>(
                fieldnames.begin(), fieldnames.end());
        }
        return used->count(name) != 0;
    };
    for (auto field : object) {
        auto key_result = field.unescaped_key();
        if (key_result.error()) {
            continue;
        }
        const std::string& key = validFieldname(key_result.value(), context);
        auto field_value_result = field.value();
        if (field_value_result.error()) {
            continue;
        }
        auto field_value = field_value_result.value();
        JsonNode fieldNode;
        if (!importJsonValue(field_value, fieldNode, context)) {
            // a value that cannot be decoded drops its field
            continue;
        }
        std::string name = key;
        if (isUsed(name)) {
            size_t index = 1;
            do {
                name = key + "_" + std::to_string(index++);
            } while (isUsed(name));
        }
        if (used) {
            used->insert(name);
        }
        fieldnames.push_back(std::move(name));
        fields.push_back(std::move(fieldNode));
    }
    return true;
}
//=============================================================================
static bool
importScalar(ondemand::value& value, JsonNode& node)
{
    // direct conversion of the usual values; anything else goes through the
    // conversion attempts below, in their historical order
    ondemand::json_type type;
    if (!value.type().get(type)) {
        switch (type) {
        case ondemand::json_type::number: {
            double number;
            if (!value.get_double().get(number)) {
                node.type = JsonNodeType::Double;
                node.scalarDouble = number;
                return true;
            }
        } break;
        case ondemand::json_type::string: {
            std::string_view text;
            if (!value.get_string().get(text)) {
                node.type = JsonNodeType::String;
                node.scalarString = text;
                return true;
            }
        } break;
        case ondemand::json_type::boolean: {
            bool flag;
            if (!value.get_bool().get(flag)) {
                node.type = JsonNodeType::Logical;
                node.scalarLogical = flag;
                return true;
            }
        } break;
        default: {
        } break;
        }
    }
    return importScalarByTrial(value, node);
}
//=============================================================================
static bool
importScalarByTrial(ondemand::value& value, JsonNode& node)
{
    {
        auto bool_result = value.get_bool();
        if (!bool_result.error()) {
            node.type = JsonNodeType::Logical;
            node.scalarLogical = bool_result.value();
            return true;
        }
    }
    {
        auto double_result = value.get_double();
        if (!double_result.error()) {
            node.type = JsonNodeType::Double;
            node.scalarDouble = double_result.value();
            return true;
        }
        if (double_result.error() == simdjson::INCORRECT_TYPE) {
            try {
                std::string_view token = value.raw_json_token();
                if (token == "null") {
                    node.type = JsonNodeType::EmptyMatrix;
                    node.scalarDouble = std::nan("");
                    return true;
                }
                if (token == "NaN" || token == "Inf" || token == "+Inf" || token == "-Inf") {
                    node.type = JsonNodeType::Double;
                    node.scalarDouble = parseJsonSpecialNumeric(token);
                    return true;
                }
            } catch (...) {
                // continue to other type checks
            }
        }
    }
    {
        auto int64_result = value.get_int64();
        if (!int64_result.error()) {
            node.type = JsonNodeType::Double;
            node.scalarDouble = static_cast<double>(int64_result.value());
            return true;
        }
        auto uint64_result = value.get_uint64();
        if (!uint64_result.error()) {
            node.type = JsonNodeType::Double;
            node.scalarDouble = static_cast<double>(uint64_result.value());
            return true;
        }
    }
    {
        auto string_result = value.get_string();
        if (!string_result.error()) {
            node.type = JsonNodeType::String;
            node.scalarString = string_result.value();
            return true;
        }
    }
    return false;
}
//=============================================================================
static JsonNodeType
commonElementType(const std::vector<JsonNode>& elements)
{
    if (elements.empty()) {
        return JsonNodeType::Undefined;
    }
    size_t doubles = 0;
    size_t logicals = 0;
    size_t empties = 0;
    size_t structs = 0;
    for (const JsonNode& element : elements) {
        switch (element.type) {
        case JsonNodeType::Double: {
            ++doubles;
        } break;
        case JsonNodeType::Logical: {
            ++logicals;
        } break;
        case JsonNodeType::EmptyMatrix: {
            ++empties;
        } break;
        case JsonNodeType::Struct: {
            ++structs;
        } break;
        default: {
            return JsonNodeType::Undefined;
        } break;
        }
    }
    const size_t total = elements.size();
    if (logicals == total) {
        return JsonNodeType::Logical;
    }
    if (structs == total) {
        return JsonNodeType::Struct;
    }
    if (doubles + empties == total) {
        // null becomes NaN in a double array
        return doubles > 0 ? JsonNodeType::Double : JsonNodeType::EmptyMatrix;
    }
    return JsonNodeType::Undefined;
}
//=============================================================================
static void
reduceNumericArray(JsonNode& node, JsonNodeType type)
{
    // element e of n, value k of each element: out[k * n + e]
    JsonComposite& composite = *node.composite;
    std::vector<JsonNode>& elements = composite.elements;
    const size_t n = elements.size();
    const bool isDouble = type == JsonNodeType::Double;
    if (composite.dims.size() == 1) {
        if (isDouble) {
            composite.doubles.resize(n);
            for (size_t e = 0; e < n; ++e) {
                composite.doubles[e] = elements[e].scalarDouble;
            }
        } else {
            composite.logicals.resize(n);
            for (size_t e = 0; e < n; ++e) {
                composite.logicals[e] = elements[e].scalarLogical;
            }
        }
    } else {
        const size_t elementSize = isDouble ? elements[0].composite->doubles.size()
                                            : elements[0].composite->logicals.size();
        if (isDouble) {
            composite.doubles.resize(n * elementSize);
        } else {
            composite.logicals.resize(n * elementSize);
        }
        for (size_t e = 0; e < n; ++e) {
            const JsonComposite& element = *elements[e].composite;
            for (size_t k = 0; k < elementSize; ++k) {
                if (isDouble) {
                    composite.doubles[k * n + e] = element.doubles[k];
                } else {
                    composite.logicals[k * n + e] = element.logicals[k];
                }
            }
        }
    }
    node.type = type;
    std::vector<JsonNode>().swap(elements);
}
//=============================================================================
static void
reduceStructArray(JsonNode& node)
{
    JsonComposite& composite = *node.composite;
    std::vector<JsonNode>& elements = composite.elements;
    const std::vector<std::string> reference = elements[0].composite->fieldnames;
    const size_t fieldCount = reference.size();
    const size_t n = elements.size();
    // position of each reference field in each element, else a cell
    std::vector<std::vector<size_t>> positions(n);
    for (size_t e = 0; e < n; ++e) {
        const std::vector<std::string>& names = elements[e].composite->fieldnames;
        if (names.size() != fieldCount) {
            node.type = JsonNodeType::Cell;
            return;
        }
        positions[e].resize(fieldCount);
        for (size_t f = 0; f < fieldCount; ++f) {
            if (names[f] == reference[f]) {
                positions[e][f] = f;
                continue;
            }
            auto it = std::find(names.begin(), names.end(), reference[f]);
            if (it == names.end()) {
                node.type = JsonNodeType::Cell;
                return;
            }
            positions[e][f] = static_cast<size_t>(it - names.begin());
        }
    }
    const size_t elementCount = elements[0].composite->count;
    const size_t count = n * elementCount;
    const bool transpose = composite.dims.size() == 2;
    composite.fields.resize(fieldCount * count);
    for (size_t e = 0; e < n; ++e) {
        JsonComposite& element = *elements[e].composite;
        for (size_t f = 0; f < fieldCount; ++f) {
            for (size_t k = 0; k < elementCount; ++k) {
                // matrices of objects are row-wise, N-D arrays element-wise
                size_t index = transpose ? k * n + e : e * elementCount + k;
                composite.fields[f * count + index]
                    = std::move(element.fields[positions[e][f] * elementCount + k]);
            }
        }
    }
    composite.fieldnames = reference;
    composite.count = count;
    node.type = JsonNodeType::Struct;
    std::vector<JsonNode>().swap(elements);
}
//=============================================================================
static const std::string&
validFieldname(std::string_view key, JsonDecodeContext& context)
{
    auto it = context.fieldnames.find(key);
    if (it != context.fieldnames.end()) {
        return it->second;
    }
    return context.fieldnames.emplace(key, MakeValidFieldname(std::string(key))).first->second;
}
//=============================================================================
double
parseJsonSpecialNumeric(std::string_view token)
{
    // "+Inf" is accepted as a token but gives NaN, as it always did
    if (token == "Inf") {
        return std::numeric_limits<double>::infinity();
    }
    if (token == "-Inf") {
        return -std::numeric_limits<double>::infinity();
    }
    return std::numeric_limits<double>::quiet_NaN();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
