//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <simdjson.h>
#include <cstring>
#include "ErrorID.hpp"
#include "StringHelpers.hpp"
#include "JsonDecode.hpp"
#include "JsonNode.hpp"
#include "JsonFileReader.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace simdjson;
//=============================================================================
static ArrayOf
jsonNodeToNelson(const JsonNode& node);
//=============================================================================
static Dimensions
nelsonDimensions(const std::vector<size_t>& dims);
//=============================================================================
static ArrayOf
jsonDecodeInternal(const simdjson::padded_string& json_padded, std::string& errorMessage);
//=============================================================================
ArrayOf
jsonDecodeFile(const std::wstring& filename, std::string& errorMessage)
{
    padded_string json_padded;
    if (!readJsonFile(filename, json_padded, errorMessage)) {
        return {};
    }
    return jsonDecodeInternal(json_padded, errorMessage);
}
//=============================================================================
ArrayOf
jsonDecode(const std::string& stringToDecode, std::string& errorMessage)
{
    return jsonDecodeInternal(simdjson::padded_string(stringToDecode), errorMessage);
}
//=============================================================================
static ArrayOf
decodeSpecialToken(std::string_view contentView, bool& decoded)
{
    std::string jsonString(contentView);
    StringHelpers::trim(jsonString);
    decoded = jsonString == "NaN" || jsonString == "Inf" || jsonString == "-Inf";
    return decoded ? ArrayOf::doubleScalar(parseJsonSpecialNumeric(jsonString)) : ArrayOf();
}
//=============================================================================
static ArrayOf
decodeScalarDocument(ondemand::document& doc, std::string& errorMessage)
{
    switch (doc.type()) {
    case simdjson::ondemand::json_type::string: {
        return ArrayOf::characterArray(std::string(doc.get_string().value()));
    } break;
    case simdjson::ondemand::json_type::number: {
        auto double_result = doc.get_double();
        if (!double_result.error()) {
            return ArrayOf::doubleScalar(double_result.value());
        }
        std::string_view content = doc.raw_json_token();
        if (content == "-Inf") {
            return ArrayOf::doubleScalar(-std::numeric_limits<double>::infinity());
        }
        // an undecoded number gives an empty 0x1 cell
        return jsonNodeToNelson(JsonNode());
    } break;
    case simdjson::ondemand::json_type::boolean: {
        auto bool_result = doc.get_bool();
        if (!bool_result.error()) {
            return ArrayOf::logicalScalar(bool_result.value());
        }
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:invalidScalarValue", "Invalid JSON scalar value.");
        return {};
    } break;
    case simdjson::ondemand::json_type::null: {
        return ArrayOf::empty();
    } break;
    default: {
        if (doc.is_alive()) {
            bool decoded = false;
            ArrayOf special = decodeSpecialToken(doc.raw_json_token(), decoded);
            if (decoded) {
                return special;
            }
        }
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:invalidScalarValue", "Invalid JSON scalar value.");
        return {};
    } break;
    }
}
//=============================================================================
static ArrayOf
jsonDecodeInternal(const simdjson::padded_string& json_padded, std::string& errorMessage)
{
    if (json_padded.size() == 0) {
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:validObjectExpected", "valid JSON Object expected.");
        return {};
    }
    simdjson::ondemand::parser parser;
    simdjson::ondemand::document doc;
    try {
        doc = parser.iterate(json_padded);
        if (doc.is_scalar()) {
            return decodeScalarDocument(doc, errorMessage);
        }
        auto root_value_result = doc.get_value();
        if (root_value_result.error()) {
            errorMessage = resolveMessageTemplate(
                "Nelson:jsondecode:parseStructureFailed", "Failed to parse JSON structure.");
            return {};
        }
        auto root_value = root_value_result.value();
        JsonNode root;
        JsonDecodeContext context;
        if (!importJsonValue(root_value, root, context)) {
            errorMessage = resolveMessageTemplate(
                "Nelson:jsondecode:validObjectExpected", "valid JSON Object expected.");
            return {};
        }
        return jsonNodeToNelson(root);
    } catch (const simdjson::simdjson_error& e) {
        if (doc.is_alive()) {
            bool decoded = false;
            ArrayOf special = decodeSpecialToken(doc.raw_json_token(), decoded);
            if (decoded) {
                return special;
            }
        }
        if (e.error() == simdjson::TAPE_ERROR) {
            errorMessage = resolveMessageTemplate("Nelson:jsondecode:invalidStructure",
                "invalid structure: missing or superfluous commas, braces, missing keys, etc.");
        } else {
            errorMessage = e.what();
        }
        return {};
    } catch (const std::runtime_error&) {
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:validObjectExpected", "valid JSON Object expected.");
        return {};
    }
}
//=============================================================================
static ArrayOf
structToNelson(const JsonComposite& composite)
{
    const stringVector& fieldnames = composite.fieldnames;
    const size_t fieldCount = fieldnames.size();
    const std::vector<size_t>& dims = composite.dims;
    if (dims.empty() || (dims.size() == 1 && dims[0] == 1)) {
        if (dims.empty() && fieldCount == 0) {
            return ArrayOf::emptyStruct();
        }
        ArrayOfVector fieldvalues;
        for (size_t f = 0; f < fieldCount; ++f) {
            fieldvalues.push_back(jsonNodeToNelson(composite.fields[f]));
        }
        return ArrayOf::structScalar(fieldnames, fieldvalues);
    }
    Dimensions structDims = nelsonDimensions(dims);
    const size_t count = composite.count;
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, count, fieldnames, false));
    for (size_t k = 0; k < count; ++k) {
        for (size_t f = 0; f < fieldCount; ++f) {
            elements[k * fieldCount + f] = jsonNodeToNelson(composite.fields[f * count + k]);
        }
    }
    return ArrayOf(NLS_STRUCT_ARRAY, structDims, elements, false, fieldnames);
}
//=============================================================================
static ArrayOf
cellToNelson(const JsonNode& node)
{
    size_t count = node.composite ? node.composite->elements.size() : 0;
    Dimensions dims(count, 1);
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, count, stringVector(), false));
    for (size_t k = 0; k < count; ++k) {
        elements[k] = jsonNodeToNelson(node.composite->elements[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOf
jsonNodeToNelson(const JsonNode& node)
{
    switch (node.type) {
    case JsonNodeType::Logical: {
        if (!node.composite) {
            return ArrayOf::logicalScalar(node.scalarLogical);
        }
        const std::vector<logical>& values = node.composite->logicals;
        Dimensions dims = nelsonDimensions(node.composite->dims);
        logical* data = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, dims.elementCount(), stringVector(), false));
        std::memcpy(data, values.data(), sizeof(logical) * values.size());
        return ArrayOf(NLS_LOGICAL, dims, data);
    } break;
    case JsonNodeType::Double: {
        if (!node.composite) {
            return ArrayOf::doubleScalar(node.scalarDouble);
        }
        const std::vector<double>& values = node.composite->doubles;
        Dimensions dims = nelsonDimensions(node.composite->dims);
        double* data = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount(), stringVector(), false));
        std::memcpy(data, values.data(), sizeof(double) * values.size());
        return ArrayOf(NLS_DOUBLE, dims, data);
    } break;
    case JsonNodeType::String: {
        return ArrayOf::characterArray(std::string(node.scalarString));
    } break;
    case JsonNodeType::EmptyMatrix: {
        return ArrayOf::empty();
    } break;
    case JsonNodeType::Struct: {
        return structToNelson(*node.composite);
    } break;
    case JsonNodeType::Undefined:
    case JsonNodeType::Array:
    case JsonNodeType::Cell:
    default: {
        return cellToNelson(node);
    } break;
    }
}
//=============================================================================
static Dimensions
nelsonDimensions(const std::vector<size_t>& dims)
{
    // a vector is a column
    if (dims.size() == 1) {
        return Dimensions(dims[0], 1);
    }
    Dimensions result;
    for (size_t k = 0; k < dims.size(); ++k) {
        result[k] = dims[k];
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
