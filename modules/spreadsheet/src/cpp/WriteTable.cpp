//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "WriteTable.hpp"
#include "TableWriter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
WriteTable(const ArrayOf& table, const std::wstring& filename, const writeTableOptions& options,
    std::wstring& errorMessage)
{
    TableWriter tableWriter;
    tableWriter.writeTableToFile(table, filename, options, errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
