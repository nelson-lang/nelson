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
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>
#include "JsonDecodeColumns.hpp"
#include "JsonFileReader.hpp"
#include "MakeValidFieldname.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace simdjson;
//=============================================================================
namespace {
    //=============================================================================
    enum class CellKind : uint8_t
    {
        Absent,
        Null,
        Number,
        Boolean,
        Text
    };
    //=============================================================================
    struct ColumnData
    {
        std::vector<CellKind> kinds;
        std::vector<double> numbers;
        std::vector<std::string_view> texts;
    };
    //=============================================================================
    struct KeySlot
    {
        std::string_view rawKey;
        size_t column;
    };
    //=============================================================================
    // Maps the keys of each object to its column. Objects usually repeat the
    // keys of the previous one in the same order: 'previous' short-cuts the
    // field name normalization for them.
    class ColumnBuilder
    {
    public:
        stringVector names;
        std::vector<ColumnData> columns;
        size_t rowCount = 0;

        bool
        addObject(dom::object object);

    private:
        std::unordered_map<std::string, size_t> columnByName;
        std::vector<KeySlot> previous;
        std::vector<size_t> objectColumns;
        bool followsPrevious = true;

        size_t
        resolveColumn(std::string_view rawKey);
    };
    //=============================================================================
    enum class Search
    {
        Found,
        NotFound,
        Ambiguous
    };
    //=============================================================================
    using DecodedFields = std::vector<std::pair<std::string, dom::element>>;
    //=============================================================================
} // namespace
//=============================================================================
static bool
locateTable(dom::element root, JsonTableLocation location, const stringVector& keys,
    dom::element& node, std::string& selector);
//=============================================================================
static bool
hasNegativeZeroInteger(const padded_string& content);
//=============================================================================
static bool
decodeTable(dom::element node, ColumnBuilder& builder);
//=============================================================================
static bool
storeValue(dom::element value, ColumnData& column, size_t row);
//=============================================================================
static ArrayOf
buildColumn(ColumnData& column, size_t rowCount);
//=============================================================================
bool
jsonDecodeColumnsFile(const std::wstring& filename, JsonTableLocation location,
    const stringVector& keys, std::string& selector, stringVector& names, ArrayOfVector& columns)
{
    padded_string content;
    std::string errorMessage;
    if (!readJsonFile(filename, content, errorMessage) || hasNegativeZeroInteger(content)) {
        return false;
    }
    dom::parser parser;
    ColumnBuilder builder;
    try {
        dom::element root;
        dom::element node;
        if (parser.parse(content).get(root)) {
            return false;
        }
        if (!locateTable(root, location, keys, node, selector)) {
            return false;
        }
        if (!decodeTable(node, builder)) {
            return false;
        }
    } catch (const std::exception&) {
        return false;
    }
    if (builder.rowCount == 0 || builder.columns.empty()) {
        return false;
    }
    names = builder.names;
    columns.clear();
    columns.reserve(builder.columns.size());
    for (auto& column : builder.columns) {
        columns.push_back(buildColumn(column, builder.rowCount));
    }
    return true;
}
//=============================================================================
bool
ColumnBuilder::addObject(dom::object object)
{
    objectColumns.clear();
    followsPrevious = true;
    for (dom::key_value_pair field : object) {
        size_t index = resolveColumn(field.key);
        if (!storeValue(field.value, columns[index], rowCount)) {
            return false;
        }
    }
    if (objectColumns.empty()) {
        return false;
    }
    previous.resize(objectColumns.size());
    rowCount++;
    return true;
}
//=============================================================================
size_t
ColumnBuilder::resolveColumn(std::string_view rawKey)
{
    size_t position = objectColumns.size();
    followsPrevious
        = followsPrevious && position < previous.size() && previous[position].rawKey == rawKey;
    if (followsPrevious) {
        objectColumns.push_back(previous[position].column);
        return previous[position].column;
    }
    // Same field names as jsondecode: valid identifier, then '_1', '_2', ...
    // for a name already used in this object.
    std::string name = MakeValidFieldname(std::string(rawKey));
    auto usedInObject = [this](const std::string& candidate) {
        for (size_t used : objectColumns) {
            if (names[used] == candidate) {
                return true;
            }
        }
        return false;
    };
    if (usedInObject(name)) {
        std::string base = name;
        size_t suffix = 1;
        do {
            name = base + "_" + std::to_string(suffix++);
        } while (usedInObject(name));
    }
    size_t index;
    auto it = columnByName.find(name);
    if (it == columnByName.end()) {
        index = columns.size();
        columnByName.emplace(name, index);
        names.push_back(name);
        columns.emplace_back();
    } else {
        index = it->second;
    }
    if (position >= previous.size()) {
        previous.resize(position + 1);
    }
    // the key view points into the parsed document, alive until the end
    previous[position].rawKey = rawKey;
    previous[position].column = index;
    objectColumns.push_back(index);
    return index;
}
//=============================================================================
static DecodedFields
decodedFields(dom::object object)
{
    // field names of the object as given by jsondecode
    DecodedFields fields;
    for (dom::key_value_pair field : object) {
        std::string name = MakeValidFieldname(std::string(field.key));
        auto used = [&fields](const std::string& candidate) {
            for (auto& decoded : fields) {
                if (decoded.first == candidate) {
                    return true;
                }
            }
            return false;
        };
        if (used(name)) {
            std::string base = name;
            size_t suffix = 1;
            do {
                name = base + "_" + std::to_string(suffix++);
            } while (used(name));
        }
        fields.emplace_back(name, field.value);
    }
    return fields;
}
//=============================================================================
static const std::pair<std::string, dom::element>*
fieldByKey(const DecodedFields& fields, const std::string& key)
{
    // key itself, then its valid field name (jsonFieldName rules)
    for (auto& field : fields) {
        if (field.first == key) {
            return &field;
        }
    }
    std::string candidate = MakeValidFieldname(key);
    for (auto& field : fields) {
        if (field.first == candidate) {
            return &field;
        }
    }
    return nullptr;
}
//=============================================================================
static bool
mayDecodeToScalarStruct(dom::array array)
{
    // [{...}] and [[...]] can give a scalar struct once decoded
    if (array.size() != 1) {
        return false;
    }
    dom::element_type type = array.at(0).type();
    return type == dom::element_type::OBJECT || type == dom::element_type::ARRAY;
}
//=============================================================================
static Search
findByName(dom::object object, const std::string& key, const std::string& prefix,
    dom::element& node, std::string& selector)
{
    DecodedFields fields = decodedFields(object);
    const auto* match = fieldByKey(fields, key);
    if (match != nullptr) {
        node = match->second;
        selector = prefix + "/" + match->first;
        return Search::Found;
    }
    for (auto& field : fields) {
        dom::object child;
        dom::array array;
        if (!field.second.get_object().get(child)) {
            Search found = findByName(child, key, prefix + "/" + field.first, node, selector);
            if (found != Search::NotFound) {
                return found;
            }
        } else if (!field.second.get_array().get(array) && mayDecodeToScalarStruct(array)) {
            return Search::Ambiguous;
        }
    }
    return Search::NotFound;
}
//=============================================================================
static Search
findFirstArrayOfObjects(
    dom::object object, const std::string& prefix, dom::element& node, std::string& selector)
{
    for (auto& field : decodedFields(object)) {
        dom::object child;
        dom::array array;
        if (!field.second.get_object().get(child)) {
            Search found
                = findFirstArrayOfObjects(child, prefix + "/" + field.first, node, selector);
            if (found != Search::NotFound) {
                return found;
            }
        } else if (!field.second.get_array().get(array)) {
            if (mayDecodeToScalarStruct(array)) {
                return Search::Ambiguous;
            }
            if (array.size() < 2) {
                continue;
            }
            bool allObjects = true;
            for (dom::element element : array) {
                dom::element_type type = element.type();
                if (type == dom::element_type::ARRAY) {
                    // may merge into a struct array once decoded
                    return Search::Ambiguous;
                }
                allObjects = allObjects && type == dom::element_type::OBJECT;
            }
            if (allObjects) {
                node = field.second;
                selector = prefix + "/" + field.first;
                return Search::Found;
            }
        }
    }
    return Search::NotFound;
}
//=============================================================================
static bool
locateTable(dom::element root, JsonTableLocation location, const stringVector& keys,
    dom::element& node, std::string& selector)
{
    selector.clear();
    node = root;
    switch (location) {
    case JsonTableLocation::Root: {
        return true;
    } break;
    case JsonTableLocation::Pointer: {
        for (const std::string& key : keys) {
            dom::object object;
            // through arrays, jsondecode indexing is left to the caller
            if (node.get_object().get(object)) {
                return false;
            }
            DecodedFields fields = decodedFields(object);
            const auto* match = fieldByKey(fields, key);
            if (match == nullptr) {
                return false;
            }
            node = match->second;
            selector += "/" + match->first;
        }
        return true;
    } break;
    case JsonTableLocation::NodeName: {
        dom::object object;
        if (keys.size() != 1 || root.get_object().get(object)) {
            return false;
        }
        return findByName(object, keys[0], "", node, selector) == Search::Found;
    } break;
    case JsonTableLocation::Automatic: {
        dom::object object;
        if (root.type() == dom::element_type::ARRAY) {
            return true;
        }
        if (root.get_object().get(object)) {
            return false;
        }
        // not found: the root object itself is the table node
        Search found = findFirstArrayOfObjects(object, "", node, selector);
        if (found == Search::NotFound) {
            node = root;
            selector.clear();
        }
        return found != Search::Ambiguous;
    } break;
    }
    return false;
}
//=============================================================================
static bool
hasNegativeZeroInteger(const padded_string& content)
{
    // simdjson DOM reads the integer -0 as 0 while jsondecode keeps -0: such
    // documents (or a "-0" inside a string) are left to jsondecode.
    const char* position = content.data();
    const char* end = content.data() + content.size();
    while (position < end) {
        position = static_cast<const char*>(std::memchr(position, '-', end - position));
        if (position == nullptr) {
            return false;
        }
        position++;
        if (position < end && *position == '0') {
            char next = position + 1 < end ? position[1] : ' ';
            bool moreDigits
                = (next >= '0' && next <= '9') || next == '.' || next == 'e' || next == 'E';
            if (!moreDigits) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
static bool
decodeTable(dom::element node, ColumnBuilder& builder)
{
    dom::object object;
    if (!node.get_object().get(object)) {
        // a single object is a single row
        return builder.addObject(object);
    }
    dom::array rows;
    if (node.get_array().get(rows)) {
        return false;
    }
    for (dom::element element : rows) {
        if (element.get_object().get(object)) {
            return false;
        }
        if (!builder.addObject(object)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
storeValue(dom::element value, ColumnData& column, size_t row)
{
    if (column.kinds.size() < row) {
        column.kinds.resize(row, CellKind::Absent);
        column.numbers.resize(row, 0.);
        column.texts.resize(row);
    }
    double number = 0.;
    std::string_view text;
    CellKind kind;
    switch (value.type()) {
    case dom::element_type::INT64:
    case dom::element_type::UINT64:
    case dom::element_type::DOUBLE: {
        if (value.get_double().get(number)) {
            return false;
        }
        kind = CellKind::Number;
    } break;
    case dom::element_type::BOOL: {
        bool flag = false;
        if (value.get_bool().get(flag)) {
            return false;
        }
        number = flag ? 1. : 0.;
        kind = CellKind::Boolean;
    } break;
    case dom::element_type::STRING: {
        if (value.get_string().get(text)) {
            return false;
        }
        kind = CellKind::Text;
    } break;
    case dom::element_type::NULL_VALUE: {
        kind = CellKind::Null;
    } break;
    default: {
        // nested array or object: not a flat table
        return false;
    } break;
    }
    column.kinds.push_back(kind);
    column.numbers.push_back(number);
    column.texts.push_back(text);
    return true;
}
//=============================================================================
static ArrayOf
buildColumn(ColumnData& column, size_t rowCount)
{
    column.kinds.resize(rowCount, CellKind::Absent);
    column.numbers.resize(rowCount, 0.);
    column.texts.resize(rowCount);
    size_t counts[5] = { 0, 0, 0, 0, 0 };
    for (CellKind kind : column.kinds) {
        counts[static_cast<size_t>(kind)]++;
    }
    size_t numbers = counts[static_cast<size_t>(CellKind::Number)];
    size_t booleans = counts[static_cast<size_t>(CellKind::Boolean)];
    size_t texts = counts[static_cast<size_t>(CellKind::Text)];
    Dimensions dims(rowCount, 1);
    if (numbers > 0 && numbers + booleans == rowCount) {
        double* values = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, rowCount, stringVector(), false));
        std::copy(column.numbers.begin(), column.numbers.end(), values);
        return ArrayOf(NLS_DOUBLE, dims, values);
    }
    if (booleans == rowCount) {
        logical* values = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, rowCount, stringVector(), false));
        for (size_t k = 0; k < rowCount; ++k) {
            values[k] = column.numbers[k] != 0.;
        }
        return ArrayOf(NLS_LOGICAL, dims, values);
    }
    NelsonType destinationType = texts == rowCount ? NLS_STRING_ARRAY : NLS_CELL_ARRAY;
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(destinationType, rowCount, stringVector(), false));
    for (size_t k = 0; k < rowCount; ++k) {
        switch (column.kinds[k]) {
        case CellKind::Number: {
            elements[k] = ArrayOf::doubleScalar(column.numbers[k]);
        } break;
        case CellKind::Boolean: {
            elements[k] = ArrayOf::logicalScalar(column.numbers[k] != 0.);
        } break;
        case CellKind::Text: {
            elements[k] = ArrayOf::characterArray(std::string(column.texts[k]));
        } break;
        default: {
            elements[k] = ArrayOf::empty();
        } break;
        }
    }
    return ArrayOf(destinationType, dims, elements);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
