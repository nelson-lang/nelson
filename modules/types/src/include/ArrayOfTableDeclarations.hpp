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
// This declaration fragment is included inside class ArrayOf.
// Do not include it directly from other headers.
//=============================================================================
// Table class object
//=============================================================================
#define NLS_TABLE_TYPE "table"
/**
 * Return whether the value stores table data.
 */
[[nodiscard]] bool
isTable() const;
/**
 * Return whether the value stores timetable data.
 */
[[nodiscard]] bool
isTimetable() const;
/**
 * Construct a table value from column arrays and optional labels.
 *
 * Column arrays must agree on height. variableNames labels columns and
 * rowNames labels rows when those vectors are provided.
 */
static ArrayOf
table(const ArrayOfVector& columnValues, const stringVector& variableNames,
    const stringVector& rowNames);
/**
 * Construct a timetable value from an existing table and row times.
 */
static ArrayOf
timetable(const ArrayOf& tableValue, const ArrayOf& rowTimes);
/**
 * Return the internal table data structure.
 */
ArrayOf
tableData() const;
/**
 * Return the internal table properties structure.
 */
ArrayOf
tableProperties() const;
/**
 * Replace the internal table data structure.
 */
void
setTableData(const ArrayOf& data);
/**
 * Replace the internal table properties structure.
 */
void
setTableProperties(const ArrayOf& properties);
/**
 * Return table variable names.
 */
stringVector
tableVariableNames() const;
/**
 * Return table variable type names.
 */
stringVector
tableVariableTypes() const;
/**
 * Return table row names.
 */
stringVector
tableRowNames() const;
/**
 * Return the number of table variables.
 */
indexType
tableWidth() const;
/**
 * Return the number of table rows.
 */
indexType
tableHeight() const;
/**
 * Return table dimensions as [height, width].
 */
Dimensions
tableDimensions() const;
/**
 * Return whether the table has no rows or no variables.
 */
bool
isTableEmpty() const;
/**
 * Return a table variable by name.
 */
ArrayOf
tableColumn(const std::string& columnName) const;
/**
 * Return a table variable by zero-based index.
 */
ArrayOf
tableColumn(indexType columnIndex) const;
/**
 * Replace an existing table variable by name.
 */
void
setTableColumn(const std::string& columnName, const ArrayOf& columnData);
/**
 * Add a table variable.
 *
 * The new variable must have a valid name and a height compatible with the
 * existing table.
 */
void
addTableColumn(const std::string& columnName, const ArrayOf& columnData);
//=============================================================================
