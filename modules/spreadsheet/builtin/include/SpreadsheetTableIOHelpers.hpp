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
#include "DetectImportOptions.hpp"
#include "WriteTable.hpp"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
std::string
normalizedNameValueName(const ArrayOf& name);
//=============================================================================
ArrayOf
readTableWithImportOptions(Evaluator* eval, const std::wstring& filename,
    const ArrayOfVector& argIn, size_t optionsIndex, size_t& firstNameValue);
//=============================================================================
ArrayOf
normalizeImportedTimeTextColumns(const ArrayOf& table, const std::wstring& inputFormat = L"");
//=============================================================================
void
initializeWriteTableOptions(writeTableOptions& options);
//=============================================================================
void
parseWriteTableOptions(const ArrayOfVector& argIn, size_t startIdx, writeTableOptions& options);
//=============================================================================
std::string
determineWriteFileType(const std::wstring& filename, const std::string& specifiedType);
//=============================================================================
void
validateWriteTableOptions(writeTableOptions& options);
//=============================================================================
ArrayOf
makeDatetime(const std::vector<double>& serial);
//=============================================================================
ArrayOf
makeDuration(const std::vector<double>& seconds);
//=============================================================================
bool
cellText(const ArrayOf& column, wstringVector& text);
//=============================================================================
bool
parseTextAsDatetime(const wstringVector& text, const std::wstring& inputFormat, ArrayOf& rowTimes);
//=============================================================================
bool
parseTextAsDuration(const wstringVector& text, ArrayOf& rowTimes);
//=============================================================================
bool
isTimeObject(const ArrayOf& value);
//=============================================================================
indexType
timeLength(const ArrayOf& value);
//=============================================================================
double
scalarTimeStorage(const ArrayOf& value, const std::string& field);
//=============================================================================
ArrayOf
timeObjectToText(const ArrayOf& value);
//=============================================================================
} // namespace Nelson
//=============================================================================
