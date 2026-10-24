//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ParquetRead.hpp"
#include "ParquetHelpers.hpp"
#include "ParquetRowGroupFilter.hpp"
#include <arrow/array/util.h>
#include <arrow/io/api.h>
#include <parquet/arrow/reader.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::shared_ptr<arrow::Table>
makeEmptyTable(const std::shared_ptr<arrow::Schema>& schema, std::wstring& errorMessage)
{
    std::vector<std::shared_ptr<arrow::Array>> arrays;
    arrays.reserve(schema->num_fields());
    for (int k = 0; k < schema->num_fields(); ++k) {
        auto array = arrow::MakeEmptyArray(schema->field(k)->type());
        if (!array.ok()) {
            errorMessage = toWideError(array.status().ToString());
            return nullptr;
        }
        arrays.push_back(*array);
    }
    return arrow::Table::Make(schema, arrays);
}
//=============================================================================
static std::shared_ptr<arrow::Table>
applySelectedNames(const std::shared_ptr<arrow::Table>& table, const stringVector& selectedNames,
    std::wstring& errorMessage)
{
    if (selectedNames.empty()) {
        return table;
    }
    std::vector<int> columnIndices
        = selectedColumnIndices(table->schema(), selectedNames, errorMessage);
    if (!errorMessage.empty()) {
        return nullptr;
    }
    auto selected = table->SelectColumns(columnIndices);
    if (!selected.ok()) {
        errorMessage = toWideError(selected.status().ToString());
        return nullptr;
    }
    return *selected;
}
//=============================================================================
static ArrayOf
tableFromArrowTable(const std::shared_ptr<arrow::Table>& arrowTable, std::wstring& errorMessage)
{
    ArrayOfVector columns;
    stringVector names;
    for (int k = 0; k < arrowTable->num_columns(); ++k) {
        auto field = arrowTable->schema()->field(k);
        ArrayOf column = nelsonColumnFromArrowArray(arrowTable->column(k), field, errorMessage);
        if (!errorMessage.empty()) {
            return {};
        }
        columns.push_back(column);
        names.push_back(field->name());
    }
    return ArrayOf::table(columns, names, {});
}
//=============================================================================
ArrayOf
ParquetRead(const std::wstring& filename, const stringVector& selectedNames,
    const std::vector<int>& rowGroups, const ArrayOf& rowFilterExpression,
    std::wstring& errorMessage)
{
    auto input = arrow::io::ReadableFile::Open(toUtf8Path(filename));
    if (!input.ok()) {
        errorMessage = toWideError(input.status().ToString());
        return {};
    }

    auto readerResult = parquet::arrow::OpenFile(*input, arrow::default_memory_pool());
    if (!readerResult.ok()) {
        errorMessage = toWideError(readerResult.status().ToString());
        return {};
    }
    std::unique_ptr<parquet::arrow::FileReader> reader = std::move(*readerResult);

    std::shared_ptr<arrow::Schema> schema;
    auto status = reader->GetSchema(&schema);
    if (!status.ok()) {
        errorMessage = toWideError(status.ToString());
        return {};
    }

    bool hasRowGroupSelection = false;
    std::vector<int> selectedRowGroups = selectParquetRowGroupsByFilter(
        reader->parquet_reader()->metadata(), rowFilterExpression, rowGroups, hasRowGroupSelection);

    std::shared_ptr<arrow::Table> arrowTable;
    if (hasRowGroupSelection && selectedRowGroups.empty()) {
        arrowTable = makeEmptyTable(schema, errorMessage);
        if (!errorMessage.empty()) {
            return {};
        }
    } else {
        auto tableResult
            = hasRowGroupSelection ? reader->ReadRowGroups(selectedRowGroups) : reader->ReadTable();
        if (!tableResult.ok()) {
            errorMessage = toWideError(tableResult.status().ToString());
            return {};
        }
        arrowTable = std::move(*tableResult);
    }

    arrowTable = applySelectedNames(arrowTable, selectedNames, errorMessage);
    if (!errorMessage.empty()) {
        return {};
    }
    return tableFromArrowTable(arrowTable, errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
