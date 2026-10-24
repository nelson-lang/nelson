//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParquetWrite.hpp"
#include "ParquetHelpers.hpp"
#include <arrow/io/api.h>
#include <parquet/arrow/writer.h>
#include <parquet/properties.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
zeroVariableTablePath(const ArrayOf& tabularValue, const std::string& path);
//=============================================================================
static parquet::Compression::type
compressionFromName(const std::string& name)
{
    if (name == "snappy") {
        return parquet::Compression::SNAPPY;
    }
    if (name == "gzip") {
        return parquet::Compression::GZIP;
    }
    if (name == "brotli") {
        return parquet::Compression::BROTLI;
    }
    if (name == "zstd") {
        return parquet::Compression::ZSTD;
    }
    if (name == "lz4") {
        return parquet::Compression::LZ4;
    }
    return parquet::Compression::UNCOMPRESSED;
}
//=============================================================================
static std::shared_ptr<parquet::WriterProperties>
makeWriterProperties(const ParquetWriteOptions& options)
{
    parquet::WriterProperties::Builder builder;
    builder.compression(compressionFromName(options.compression));
    if (options.version == "1.0") {
        builder.version(parquet::ParquetVersion::PARQUET_1_0);
    } else if (options.version == "2.4") {
        builder.version(parquet::ParquetVersion::PARQUET_2_4);
    } else {
        builder.version(parquet::ParquetVersion::PARQUET_2_6);
    }
    if (options.encoding == "plain") {
        builder.disable_dictionary();
    } else if (options.encoding == "dictionary") {
        builder.enable_dictionary();
    }
    return builder.build();
}
//=============================================================================
static bool
appendColumnValue(const ArrayOf& column, const std::string& name, ArrayOfVector& values,
    std::vector<std::shared_ptr<arrow::Field>>& fields,
    std::vector<std::shared_ptr<arrow::Array>>& arrays, std::wstring& errorMessage)
{
    auto type = arrowTypeForNelsonColumn(column, errorMessage);
    if (!errorMessage.empty()) {
        return false;
    }
    auto array = arrowArrayFromNelsonColumn(column, errorMessage);
    if (!errorMessage.empty()) {
        return false;
    }
    values.push_back(column);
    fields.push_back(arrow::field(name, type));
    arrays.push_back(array);
    return true;
}
//=============================================================================
static std::shared_ptr<arrow::Table>
makeArrowTable(const ArrayOf& tableValue, std::wstring& errorMessage)
{
    auto columns = tabularColumns(tableValue);
    ArrayOfVector values;
    std::vector<std::shared_ptr<arrow::Field>> fields;
    std::vector<std::shared_ptr<arrow::Array>> arrays;
    fields.reserve(columns.size());
    arrays.reserve(columns.size());
    for (const auto& [name, value] : columns) {
        if (!appendColumnValue(value, name, values, fields, arrays, errorMessage)) {
            return nullptr;
        }
    }
    return arrow::Table::Make(arrow::schema(fields), arrays);
}
//=============================================================================
static int64_t
rowGroupLength(const ParquetWriteOptions& options, const std::shared_ptr<arrow::Table>& table)
{
    if (!options.rowGroupHeights.empty() && options.rowGroupHeights[0] > 0) {
        return static_cast<int64_t>(options.rowGroupHeights[0]);
    }
    return std::max<int64_t>(1, table->num_rows());
}
//=============================================================================
void
ParquetWrite(const std::wstring& filename, const ArrayOf& tableValue,
    const ParquetWriteOptions& options, std::wstring& errorMessage)
{
    if (!tableValue.isTable() && !tableValue.isTimetable()) {
        errorMessage = L"Second argument must be a table or timetable.";
        return;
    }
    // A Parquet file cannot keep the height of a table without variables.
    ArrayOf tableSource = tableValue.isTimetable() ? tableValue.field("Table") : tableValue;
    if (tableSource.tableVariableNames().empty()) {
        errorMessage = L"Second argument must have at least one variable.";
        return;
    }
    std::string zeroVariablePath
        = zeroVariableTablePath(tableValue, tableValue.isTimetable() ? "TT" : "T");
    if (!zeroVariablePath.empty()) {
        errorMessage = utf8_to_wstring(zeroVariablePath)
            + L" is a table with zero variables. Zero variable tables are not supported.";
        return;
    }
    auto table = makeArrowTable(tableValue, errorMessage);
    if (!errorMessage.empty()) {
        return;
    }
    auto output = arrow::io::FileOutputStream::Open(toUtf8Path(filename));
    if (!output.ok()) {
        errorMessage = toWideError(output.status().ToString());
        return;
    }
    auto writerOptions = makeWriterProperties(options);
    // Parquet has no duration type: the stored Arrow schema lets the reader restore it.
    auto arrowOptions = parquet::ArrowWriterProperties::Builder().store_schema()->build();
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), *output,
        rowGroupLength(options, table), writerOptions, arrowOptions);
    if (!status.ok()) {
        errorMessage = toWideError(status.ToString());
    }
}
//=============================================================================
// Path of the first nested table without variables: its height cannot be kept.
std::string
zeroVariableTablePath(const ArrayOf& tabularValue, const std::string& path)
{
    for (const auto& [name, value] : tabularColumns(tabularValue)) {
        if (!value.isTable() && !value.isTimetable()) {
            continue;
        }
        std::string valuePath = path + "." + name;
        if (value.isTable() && value.tableVariableNames().empty()) {
            return valuePath;
        }
        std::string nestedPath = zeroVariableTablePath(value, valuePath);
        if (!nestedPath.empty()) {
            return nestedPath;
        }
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
