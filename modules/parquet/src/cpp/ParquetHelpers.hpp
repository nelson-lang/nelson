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
#include "ArrayOf.hpp"
#include "characters_encoding.hpp"
#include <arrow/api.h>
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr double SERIAL_UNIX_EPOCH = 719529.0;
constexpr double SECONDS_PER_DAY = 86400.0;
//=============================================================================
std::wstring
toWideError(const std::string& message);
//=============================================================================
std::string
toUtf8Path(const std::wstring& filename);
//=============================================================================
bool
isDuration(const ArrayOf& value);
//=============================================================================
bool
isDatetime(const ArrayOf& value);
//=============================================================================
ArrayOf
makeDatetimeColumn(const std::vector<double>& serialValues);
//=============================================================================
ArrayOf
makeDurationColumn(const std::vector<double>& secondsValues);
//=============================================================================
std::shared_ptr<arrow::DataType>
arrowTypeForNelsonColumn(const ArrayOf& column, std::wstring& errorMessage);
//=============================================================================
std::shared_ptr<arrow::Array>
arrowArrayFromNelsonColumn(const ArrayOf& column, std::wstring& errorMessage);
//=============================================================================
// Columns of a table, or of a timetable with its row times first.
std::vector<std::pair<std::string, ArrayOf>>
tabularColumns(const ArrayOf& tabularValue);
//=============================================================================
ArrayOf
nelsonColumnFromArrowArray(const std::shared_ptr<arrow::ChunkedArray>& column,
    const std::shared_ptr<arrow::Field>& field, std::wstring& errorMessage);
//=============================================================================
std::vector<int>
selectedColumnIndices(const std::shared_ptr<arrow::Schema>& schema,
    const stringVector& selectedNames, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
