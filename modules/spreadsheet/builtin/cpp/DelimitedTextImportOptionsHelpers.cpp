//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DelimitedTextImportOptionsHelpers.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::SpreadsheetGateway {
//=============================================================================
static stringVector
defaultVariableTypes(size_t n)
{
    stringVector types;
    types.resize(n, "char");
    return types;
}
//=============================================================================
static stringVector
cellstrOrEmpty(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return {};
    }
    // A property assigned directly (e.g. opts.Delimiter = ',') keeps the raw
    // value, which may be a character row vector rather than a cell or string
    // array; accept it as a single entry.
    if (value.isRowVectorCharacterArray()) {
        return { value.asUtf8String() };
    }
    if (!value.isCell() && !value.isStringArray()) {
        return {};
    }
    return value.asUtf8StringRowVector();
}
//=============================================================================
bool
isDelimitedTextImportOptions(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == NELSON_DELIMITED_TEXT_IMPORT_OPTIONS_CLASS;
}
//=============================================================================
bool
isSpreadsheetImportOptions(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == NELSON_SPREADSHEET_IMPORT_OPTIONS_CLASS;
}
//=============================================================================
bool
isImportOptions(const ArrayOf& value)
{
    return isDelimitedTextImportOptions(value) || isSpreadsheetImportOptions(value);
}
//=============================================================================
ArrayOf
toDelimitedTextImportOptions(const detectImportOptions& options)
{
    const std::string className = NELSON_DELIMITED_TEXT_IMPORT_OPTIONS_CLASS;
    if (!classdefLoadClass(className)) {
        Error("Nelson:spreadsheet:valueClassNotFound", "Value class not found: %s", className);
    }

    stringVector variableTypes = defaultVariableTypes(options.VariableNames.size());
    stringVector fieldnames = { "SelectedVariableNames", "VariableNames", "VariableOptions",
        "VariableTypes", "ImportErrorRule", "MissingRule", "PreserveVariableNames",
        "VariableNamingRule", "DataLines", "VariableNamesLine", "RowNamesColumn",
        "VariableUnitsLine", "VariableDescriptionsLine", "ExtraColumnsRule", "EmptyLineRule",
        "Whitespace", "LineEnding", "CommentStyle", "Encoding", "ConsecutiveDelimitersRule",
        "LeadingDelimitersRule", "TrailingDelimitersRule", "Delimiter" };

    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(options.VariableNames);
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(options.VariableNames);
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector());
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(variableTypes);
    fieldvalues << ArrayOf::characterArray("fill");
    fieldvalues << ArrayOf::characterArray("fill");
    fieldvalues << ArrayOf::logicalScalar(false);
    fieldvalues << ArrayOf::characterArray("modify");
    fieldvalues << ArrayOf::doubleRowVector(options.DataLines);
    fieldvalues << ArrayOf::doubleScalar(options.VariableNamesLine);
    fieldvalues << ArrayOf::doubleScalar(options.RowNamesColumn);
    fieldvalues << ArrayOf::doubleScalar(0);
    fieldvalues << ArrayOf::doubleScalar(0);
    fieldvalues << ArrayOf::characterArray("addvars");
    fieldvalues << ArrayOf::characterArray(options.EmptyLineRule);
    fieldvalues << ArrayOf::characterArray("\b\t ");
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(options.LineEnding);
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(options.CommentStyle);
    fieldvalues << ArrayOf::characterArray("");
    fieldvalues << ArrayOf::characterArray("split");
    fieldvalues << ArrayOf::characterArray("keep");
    fieldvalues << ArrayOf::characterArray("keep");
    fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(options.Delimiter);

    ArrayOf object = ArrayOf::classdefArray(className, fieldnames, fieldvalues);
    object.setClassdefLayout(classdefLayout(className));
    return object;
}
//=============================================================================
static std::string
structEntryText(const ArrayOfVector& list, size_t index)
{
    if (index >= list.size()) {
        return "";
    }
    const ArrayOf& value = list[index];
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return value.asUtf8String();
    }
    return "";
}
//=============================================================================
void
populateVariableImportOptions(const ArrayOf& importOptionsObj, detectImportOptions& options)
{
    options.VariableNames = cellstrOrEmpty(importOptionsObj.field("VariableNames"));
    options.VariableTypes = cellstrOrEmpty(importOptionsObj.field("VariableTypes"));
    size_t nvars = options.VariableTypes.size();
    options.VariableInputFormats.assign(nvars, "");
    options.VariableWhitespaceRules.assign(nvars, "");
    if (!importOptionsObj.isField("VariableOptions")) {
        return;
    }
    ArrayOf variableOptions = importOptionsObj.field("VariableOptions");
    if (!variableOptions.isStruct() || variableOptions.elementCount() != nvars) {
        return;
    }
    ArrayOfVector inputFormats;
    ArrayOfVector whitespaceRules;
    if (variableOptions.isField("InputFormat")) {
        inputFormats = variableOptions.fieldAsList("InputFormat");
    }
    if (variableOptions.isField("WhitespaceRule")) {
        whitespaceRules = variableOptions.fieldAsList("WhitespaceRule");
    }
    for (size_t k = 0; k < nvars; ++k) {
        options.VariableInputFormats[k] = structEntryText(inputFormats, k);
        options.VariableWhitespaceRules[k] = structEntryText(whitespaceRules, k);
    }
}
//=============================================================================
void
populateImportOptions(const ArrayOf& importOptionsObj, detectImportOptions& options)
{
    options.Delimiter = cellstrOrEmpty(importOptionsObj.field("Delimiter"));
    options.LineEnding = cellstrOrEmpty(importOptionsObj.field("LineEnding"));
    options.CommentStyle = cellstrOrEmpty(importOptionsObj.field("CommentStyle"));
    options.EmptyLineRule = importOptionsObj.field("EmptyLineRule").asUtf8String();
    options.VariableNamesLine = (int)importOptionsObj.field("VariableNamesLine").asDoubleScalar();
    options.RowNamesColumn = (int)importOptionsObj.field("RowNamesColumn").asDoubleScalar();
    options.DataLines = importOptionsObj.field("DataLines").asDoubleVector();
    populateVariableImportOptions(importOptionsObj, options);
}
//=============================================================================
} // namespace Nelson::SpreadsheetGateway
//=============================================================================
