//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParquetInfo.hpp"
#include "ParquetHelpers.hpp"
#include <algorithm>
#include <arrow/io/api.h>
#include <cctype>
#include <parquet/file_reader.h>
#include <parquet/arrow/reader.h>
#include <parquet/metadata.h>
#include <parquet/types.h>
#include <filesystem>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
makeRowGroups(const std::shared_ptr<parquet::FileMetaData>& metadata)
{
    std::vector<double> rows;
    rows.reserve(metadata->num_row_groups());
    for (int k = 0; k < metadata->num_row_groups(); ++k) {
        rows.push_back(static_cast<double>(metadata->RowGroup(k)->num_rows()));
    }
    Dimensions dims(rows.size(), 1);
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rows.size()));
    std::copy(rows.begin(), rows.end(), data);
    return ArrayOf(NLS_DOUBLE, dims, data);
}
//=============================================================================
static std::string
compressionToString(parquet::Compression::type value)
{
    switch (value) {
    case parquet::Compression::UNCOMPRESSED:
        return "uncompressed";
    case parquet::Compression::SNAPPY:
        return "snappy";
    case parquet::Compression::GZIP:
        return "gzip";
    case parquet::Compression::BROTLI:
        return "brotli";
    case parquet::Compression::ZSTD:
        return "zstd";
    case parquet::Compression::LZ4:
        return "lz4";
    default:
        return "unknown";
    }
}
//=============================================================================
static std::string
typeToString(const std::shared_ptr<arrow::DataType>& type)
{
    switch (type->id()) {
    case arrow::Type::BOOL:
        return "logical";
    case arrow::Type::INT8:
        return "int8";
    case arrow::Type::UINT8:
        return "uint8";
    case arrow::Type::INT16:
        return "int16";
    case arrow::Type::UINT16:
        return "uint16";
    case arrow::Type::INT32:
        return "int32";
    case arrow::Type::UINT32:
        return "uint32";
    case arrow::Type::INT64:
        return "int64";
    case arrow::Type::UINT64:
        return "uint64";
    case arrow::Type::FLOAT:
        return "single";
    case arrow::Type::DOUBLE:
        return "double";
    case arrow::Type::STRING:
    case arrow::Type::LARGE_STRING:
        return "string";
    case arrow::Type::TIMESTAMP:
        return "datetime";
    case arrow::Type::DURATION:
        return "duration";
    case arrow::Type::LIST:
        return "cell";
    case arrow::Type::STRUCT:
        return "table";
    default:
        return type->ToString();
    }
}
//=============================================================================
static std::string
encodingToString(const parquet::ColumnChunkMetaData* chunk)
{
    if (!chunk) {
        return "";
    }
    const auto& encodings = chunk->encodings();
    if (std::find(encodings.begin(), encodings.end(), parquet::Encoding::RLE_DICTIONARY)
            != encodings.end()
        || std::find(encodings.begin(), encodings.end(), parquet::Encoding::PLAIN_DICTIONARY)
            != encodings.end()) {
        return "dictionary";
    }
    if (std::find(encodings.begin(), encodings.end(), parquet::Encoding::PLAIN)
        != encodings.end()) {
        return "plain";
    }
    if (!encodings.empty()) {
        std::string value = parquet::EncodingToString(encodings.front());
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }
    return "";
}
//=============================================================================
static int
findColumnIndex(const std::shared_ptr<parquet::FileMetaData>& metadata, const std::string& name)
{
    for (int k = 0; k < metadata->num_columns(); ++k) {
        std::string path = metadata->schema()->Column(k)->path()->ToDotString();
        if (path == name || path.rfind(name + ".", 0) == 0) {
            return k;
        }
    }
    return -1;
}
//=============================================================================
static ArrayOf
stringColumn(const stringVector& values)
{
    Dimensions dims(values.size(), 1);
    return ArrayOf::stringArray(values, dims);
}
//=============================================================================
static ArrayOf
makeVariableInfo(const std::shared_ptr<parquet::FileMetaData>& metadata,
    const std::shared_ptr<arrow::Schema>& schema)
{
    stringVector names;
    stringVector types;
    stringVector compression;
    stringVector encoding;
    int columns = schema ? schema->num_fields() : metadata->num_columns();
    for (int k = 0; k < columns; ++k) {
        std::string name
            = schema ? schema->field(k)->name() : metadata->schema()->Column(k)->name();
        int parquetColumn = findColumnIndex(metadata, name);
        names.push_back(name);
        types.push_back(schema
                ? typeToString(schema->field(k)->type())
                : parquet::TypeToString(metadata->schema()->Column(k)->physical_type()));
        auto chunk = metadata->num_row_groups() > 0 && parquetColumn >= 0
            ? metadata->RowGroup(0)->ColumnChunk(parquetColumn)
            : nullptr;
        compression.push_back(chunk ? compressionToString(chunk->compression()) : "");
        encoding.push_back(encodingToString(chunk.get()));
    }
    ArrayOfVector values;
    values.push_back(stringColumn(names));
    values.push_back(stringColumn(types));
    values.push_back(stringColumn(compression));
    values.push_back(stringColumn(encoding));
    return ArrayOf::structScalar(
        stringVector { "Name", "Type", "Compression", "Encoding" }, values);
}
//=============================================================================
static std::shared_ptr<arrow::Schema>
readArrowSchema(const std::wstring& filename)
{
    auto input = arrow::io::ReadableFile::Open(toUtf8Path(filename));
    if (!input.ok()) {
        return nullptr;
    }
    auto readerResult = parquet::arrow::OpenFile(*input, arrow::default_memory_pool());
    if (!readerResult.ok()) {
        return nullptr;
    }
    std::unique_ptr<parquet::arrow::FileReader> reader = std::move(*readerResult);
    std::shared_ptr<arrow::Schema> schema;
    if (!reader->GetSchema(&schema).ok()) {
        return nullptr;
    }
    return schema;
}
//=============================================================================
static std::string
versionToString(const std::shared_ptr<parquet::FileMetaData>& metadata)
{
    return metadata->version() <= 1 ? "1.0" : "2.0";
}
//=============================================================================
ArrayOf
ParquetInfo(const std::wstring& filename, std::wstring& errorMessage)
{
    auto input = arrow::io::ReadableFile::Open(toUtf8Path(filename));
    if (!input.ok()) {
        errorMessage = toWideError(input.status().ToString());
        return {};
    }
    auto reader = parquet::ParquetFileReader::Open(*input);
    if (!reader) {
        errorMessage = L"Unable to open Parquet file.";
        return {};
    }
    auto metadata = reader->metadata();
    std::uintmax_t size = 0;
    try {
        size = std::filesystem::file_size(toUtf8Path(filename));
    } catch (...) {
        size = 0;
    }
    auto schema = readArrowSchema(filename);
    ArrayOf rowGroups = makeRowGroups(metadata);
    ArrayOf variables = makeVariableInfo(metadata, schema);
    ArrayOfVector values;
    values.push_back(ArrayOf::characterArray(filename));
    values.push_back(ArrayOf::doubleScalar(static_cast<double>(size)));
    values.push_back(ArrayOf::doubleScalar(static_cast<double>(metadata->num_rows())));
    values.push_back(ArrayOf::doubleScalar(
        static_cast<double>(schema ? schema->num_fields() : metadata->num_columns())));
    values.push_back(ArrayOf::doubleScalar(static_cast<double>(metadata->num_row_groups())));
    values.push_back(rowGroups);
    values.push_back(rowGroups);
    values.push_back(variables);
    values.push_back(ArrayOf::characterArray(metadata->created_by()));
    values.push_back(ArrayOf::characterArray(versionToString(metadata)));
    return ArrayOf::structScalar(
        { "Filename", "FileSize", "NumRows", "NumVariables", "NumRowGroups", "RowGroups",
            "RowGroupHeights", "Variables", "CreatedBy", "Version" },
        values);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
