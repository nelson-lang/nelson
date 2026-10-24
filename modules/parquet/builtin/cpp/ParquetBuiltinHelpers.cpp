//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParquetBuiltinHelpers.hpp"
//=============================================================================
namespace Nelson::ParquetGateway {
//=============================================================================
stringVector
getStringList(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return {};
    }
    return value.asUtf8StringVector();
}
//=============================================================================
std::vector<int>
getRowGroups(const ArrayOf& value)
{
    std::vector<int> groups;
    if (value.isEmpty()) {
        return groups;
    }
    std::vector<double> raw = value.asDoubleVector();
    groups.reserve(raw.size());
    for (double group : raw) {
        groups.push_back(static_cast<int>(group) - 1);
    }
    return groups;
}
//=============================================================================
static std::string
optionalStringField(
    const ArrayOf& options, const std::string& fieldName, const std::string& defaultValue)
{
    if (!options.isStruct() || options.fieldIndex(fieldName) < 0) {
        return defaultValue;
    }
    ArrayOf value = options.field(fieldName);
    if (value.isEmpty()) {
        return defaultValue;
    }
    return value.asUtf8String();
}
//=============================================================================
ParquetWriteOptions
getWriteOptions(const ArrayOf& value)
{
    ParquetWriteOptions options;
    options.compression = optionalStringField(value, "Compression", "snappy");
    options.encoding = optionalStringField(value, "Encoding", "auto");
    options.version = optionalStringField(value, "Version", "2.6");
    if (value.isStruct() && value.fieldIndex("RowGroupHeights") >= 0) {
        std::vector<double> raw = value.field("RowGroupHeights").asDoubleVector();
        for (double height : raw) {
            options.rowGroupHeights.push_back(static_cast<int64>(height));
        }
    }
    return options;
}
//=============================================================================
} // namespace Nelson::ParquetGateway
//=============================================================================
