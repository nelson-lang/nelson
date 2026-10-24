//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uihtml_spikeBuiltin.hpp"
#include "UiHtmlHost.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
// Spike driver for the future `uihtml` component. Deliberately named with
// leading underscores: `uihtml` itself belongs to the graphics module and this
// must never be mistaken for it.
//=============================================================================
namespace Nelson::WebviewGateway {
//=============================================================================
static size_t
identifierArgument(const ArrayOfVector& argIn, size_t index);
//=============================================================================
static std::string
textArgument(const ArrayOfVector& argIn, size_t index);
//=============================================================================
static void
raiseHostError(const std::wstring& error);
//=============================================================================
static ArrayOf
statsToStruct(const UiHtmlStats& stats);
//=============================================================================
static ArrayOf
toCellOfCharacterArrays(const std::vector<std::string>& values);
//=============================================================================
ArrayOfVector
uihtml_spikeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval;
    const std::wstring command = utf8_to_wstring(textArgument(argIn, 0));
    std::wstring error;

    if (command == L"open") {
        nargincheck(argIn, 2, 2);
        size_t id = 0;
        if (!UiHtmlOpen(utf8_to_wstring(textArgument(argIn, 1)), id, error)) {
            raiseHostError(error);
        }
        retval << ArrayOf::doubleScalar(static_cast<double>(id));
        return retval;
    }
    if (command == L"source") {
        nargincheck(argIn, 3, 3);
        if (!UiHtmlSetSource(
                identifierArgument(argIn, 1), utf8_to_wstring(textArgument(argIn, 2)), error)) {
            raiseHostError(error);
        }
        return retval;
    }
    if (command == L"setdata") {
        nargincheck(argIn, 3, 3);
        if (!UiHtmlSetData(identifierArgument(argIn, 1), textArgument(argIn, 2), error)) {
            raiseHostError(error);
        }
        return retval;
    }
    if (command == L"getdata") {
        nargincheck(argIn, 2, 2);
        std::string dataJson;
        if (!UiHtmlGetData(identifierArgument(argIn, 1), dataJson, error)) {
            raiseHostError(error);
        }
        retval << ArrayOf::characterArray(dataJson);
        return retval;
    }
    if (command == L"sendevent") {
        nargincheck(argIn, 4, 4);
        if (!UiHtmlSendEvent(identifierArgument(argIn, 1), textArgument(argIn, 2),
                textArgument(argIn, 3), error)) {
            raiseHostError(error);
        }
        return retval;
    }
    if (command == L"events") {
        nargincheck(argIn, 2, 2);
        std::vector<UiHtmlEventRecord> events;
        if (!UiHtmlDrainEvents(identifierArgument(argIn, 1), events, error)) {
            raiseHostError(error);
        }
        std::vector<std::string> names;
        std::vector<std::string> payloads;
        for (const UiHtmlEventRecord& record : events) {
            names.push_back(record.name);
            payloads.push_back(record.dataJson);
        }
        retval << toCellOfCharacterArrays(names);
        if (nLhs > 1) {
            retval << toCellOfCharacterArrays(payloads);
        }
        return retval;
    }
    if (command == L"paths") {
        nargincheck(argIn, 2, 2);
        std::vector<std::string> paths;
        if (!UiHtmlGetRequestedPaths(identifierArgument(argIn, 1), paths, error)) {
            raiseHostError(error);
        }
        retval << toCellOfCharacterArrays(paths);
        return retval;
    }
    if (command == L"stats") {
        nargincheck(argIn, 2, 2);
        UiHtmlStats stats;
        if (!UiHtmlGetStats(identifierArgument(argIn, 1), stats, error)) {
            raiseHostError(error);
        }
        retval << statsToStruct(stats);
        return retval;
    }
    if (command == L"isopen") {
        nargincheck(argIn, 2, 2);
        retval << ArrayOf::logicalScalar(UiHtmlIsOpen(identifierArgument(argIn, 1)));
        return retval;
    }
    if (command == L"close") {
        nargincheck(argIn, 2, 2);
        UiHtmlClose(identifierArgument(argIn, 1));
        return retval;
    }
    if (command == L"closeall") {
        nargincheck(argIn, 1, 1);
        UiHtmlCloseAll();
        return retval;
    }
    Error(L"Nelson:uihtml_spike:unknownCommand", L"Unknown uihtml spike command.");
    return retval;
}
//=============================================================================
static size_t
identifierArgument(const ArrayOfVector& argIn, size_t index)
{
    const ArrayOf& argument = argIn[index];
    if (!argument.isScalar() || !argument.isNumeric()) {
        Error(L"Nelson:uihtml_spike:invalidIdentifier", L"A scalar uihtml identifier is expected.");
    }
    const double value = argument.asDoubleScalar();
    if (value < 1 || value != static_cast<double>(static_cast<size_t>(value))) {
        Error(L"Nelson:uihtml_spike:invalidIdentifier", L"A scalar uihtml identifier is expected.");
    }
    return static_cast<size_t>(value);
}
//=============================================================================
static std::string
textArgument(const ArrayOfVector& argIn, size_t index)
{
    const ArrayOf& argument = argIn[index];
    if (!argument.isRowVectorCharacterArray() && !argument.isScalarStringArray()) {
        Error(L"Nelson:uihtml_spike:invalidText",
            L"A character vector or a scalar string is expected.");
    }
    return wstring_to_utf8(argument.asWideString());
}
//=============================================================================
static void
raiseHostError(const std::wstring& error)
{
    Error(
        L"Nelson:uihtml_spike:hostFailure", error.empty() ? L"uihtml spike host failure." : error);
}
//=============================================================================
static ArrayOf
statsToStruct(const UiHtmlStats& stats)
{
    stringVector fieldNames;
    ArrayOfVector values;
    fieldNames.push_back("revision");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.revision));
    fieldNames.push_back("dataFromPage");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.dataFromPage));
    fieldNames.push_back("dataToPage");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.dataToPage));
    fieldNames.push_back("eventsFromPage");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.eventsFromPage));
    fieldNames.push_back("eventsToPage");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.eventsToPage));
    fieldNames.push_back("setupCount");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.setupCount));
    fieldNames.push_back("outOfOrder");
    values << ArrayOf::doubleScalar(static_cast<double>(stats.outOfOrder));
    fieldNames.push_back("bridgeAvailable");
    values << ArrayOf::logicalScalar(stats.bridgeAvailable);
    return ArrayOf::structScalar(fieldNames, values);
}
//=============================================================================
static ArrayOf
toCellOfCharacterArrays(const std::vector<std::string>& values)
{
    if (values.empty()) {
        return ArrayOf::emptyCell(Dimensions(0, 0));
    }
    Dimensions dimensions(values.size(), 1);
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size(), stringVector(), false));
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = ArrayOf::characterArray(values[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dimensions, elements);
}
//=============================================================================
} // namespace Nelson::WebviewGateway
//=============================================================================
