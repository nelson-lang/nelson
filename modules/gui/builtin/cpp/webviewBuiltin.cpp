//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "webviewBuiltin.hpp"
#include "examplesBuiltin.hpp"
#include "ExamplesCatalogHost.hpp"
#include "WebToolsHost.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebViewer.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <cmath>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::wstring
getTextArgument(const ArrayOfVector& argIn, size_t index)
{
    if (!(argIn[index].isRowVectorCharacterArray() || argIn[index].isScalarStringArray())) {
        Error(L"Nelson:webview:textArgumentExpected", L"Text argument expected.");
    }
    return argIn[index].asWideString();
}
//=============================================================================
bool
getLogicalArgument(const ArrayOfVector& argIn, size_t index)
{
    const ArrayOf& value = argIn[index];
    if (value.isLogical() && value.isScalar()) {
        return value.asLogicalScalar();
    }
    if (value.isDoubleType(true) && value.isScalar()) {
        double scalar = value.asDoubleScalar();
        if (scalar == 0. || scalar == 1.) {
            return scalar == 1.;
        }
    }
    Error(L"Nelson:webview:logicalScalarExpected", L"Logical scalar expected.");
    return false;
}
//=============================================================================
size_t
getViewerIdArgument(const ArrayOfVector& argIn, size_t index)
{
    const ArrayOf& value = argIn[index];
    if (!value.isDoubleType(true) || !value.isScalar()) {
        Error(L"Nelson:webview:viewerIdentifierExpected", L"Viewer identifier expected.");
    }
    double scalar = value.asDoubleScalar();
    if (!std::isfinite(scalar) || scalar <= 0 || std::floor(scalar) != scalar) {
        Error(L"Nelson:webview:viewerIdentifierExpected", L"Viewer identifier expected.");
    }
    return static_cast<size_t>(scalar);
}
//=============================================================================
void
errorIfNeeded(const std::wstring& error)
{
    if (error.empty()) {
        Error(L"Nelson:webview:operationFailed", L"HTML viewer operation failed.");
    }
    Error(L"Nelson:webview:renderFailed", error);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::WebviewGateway::webviewBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 3);

    std::wstring command = getTextArgument(argIn, 0);
    ArrayOfVector retval;

    if (command == L"open") {
        nargincheck(argIn, 5, 5);
        nargoutcheck(nLhs, 2, 2);
        WebViewerOptions options;
        options.newWindow = getLogicalArgument(argIn, 2);
        options.showToolbar = getLogicalArgument(argIn, 3);
        options.showAddressBox = getLogicalArgument(argIn, 4);

        size_t viewerId = 0;
        std::wstring currentInput;
        std::wstring error;
        if (!OpenWebViewer(getTextArgument(argIn, 1), options, viewerId, currentInput, error)) {
            errorIfNeeded(error);
        }
        retval << ArrayOf::doubleScalar(static_cast<double>(viewerId));
        retval << ArrayOf::characterArray(currentInput);
        return retval;
    }

    if (command == L"setInput") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 1, 1);
        std::wstring currentInput;
        std::wstring error;
        if (!SetWebViewerInput(
                getViewerIdArgument(argIn, 1), getTextArgument(argIn, 2), currentInput, error)) {
            errorIfNeeded(error);
        }
        retval << ArrayOf::characterArray(currentInput);
        return retval;
    }

    if (command == L"setVisible") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 0, 1);
        std::wstring error;
        if (!SetWebViewerVisible(
                getViewerIdArgument(argIn, 1), getLogicalArgument(argIn, 2), error)) {
            errorIfNeeded(error);
        }
        retval << ArrayOf::logicalScalar(true);
        return retval;
    }

    if (command == L"getHtmlText") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 1, 1);
        std::wstring htmlText;
        std::wstring error;
        if (!GetWebViewerHtmlText(getViewerIdArgument(argIn, 1), htmlText, error)) {
            errorIfNeeded(error);
        }
        retval << ArrayOf::characterArray(htmlText);
        return retval;
    }

    if (command == L"close") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        bool closed = CloseWebViewer(getViewerIdArgument(argIn, 1));
        retval << ArrayOf::logicalScalar(closed);
        return retval;
    }

    if (command == L"closeAll") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        CloseAllWebViewers();
        closeExamplesWindow();
        retval << ArrayOf::logicalScalar(true);
        return retval;
    }

    if (command == L"examplesState") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 3, 3);
        size_t window = 0;
        size_t generation = 0;
        std::string catalogJson;
        getExamplesWindowState(window, generation, catalogJson);
        retval << ArrayOf::doubleScalar(static_cast<double>(window));
        retval << ArrayOf::doubleScalar(static_cast<double>(generation));
        retval << ArrayOf::characterArray(utf8_to_wstring(catalogJson));
        return retval;
    }

    if (command == L"examplesCatalog") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArray(
            utf8_to_wstring(BuildLoadedModulesExamplesCatalog().json));
        return retval;
    }

    if (command == L"registerExamplesProvider") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 0, 0);
        RegisterExamplesProvider(
            getTextArgument(argIn, 1), wstring_to_utf8(getTextArgument(argIn, 2)));
        return retval;
    }

    if (command == L"unregisterExamplesProvider") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::logicalScalar(UnregisterExamplesProvider(getTextArgument(argIn, 1)));
        return retval;
    }

    if (command == L"registerWebTool") {
        nargincheck(argIn, 4, 4);
        nargoutcheck(nLhs, 0, 0);
        RegisterWebTool(wstring_to_utf8(getTextArgument(argIn, 1)), getTextArgument(argIn, 2),
            wstring_to_utf8(getTextArgument(argIn, 3)));
        return retval;
    }

    if (command == L"unregisterWebTool") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::logicalScalar(
            UnregisterWebTool(wstring_to_utf8(getTextArgument(argIn, 1))));
        return retval;
    }

    if (command == L"isWebTool") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::logicalScalar(
            IsWebToolRegistered(wstring_to_utf8(getTextArgument(argIn, 1))));
        return retval;
    }

    if (command == L"dispatchWebTool") {
        nargincheck(argIn, 4, 4);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArray(utf8_to_wstring(DispatchWebTool(
            wstring_to_utf8(getTextArgument(argIn, 1)), wstring_to_utf8(getTextArgument(argIn, 2)),
            wstring_to_utf8(getTextArgument(argIn, 3)))));
        return retval;
    }

    if (command == L"getBackend") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 1, 1);
        std::wstring backend;
        std::wstring error;
        if (!GetWebViewerBackend(getViewerIdArgument(argIn, 1), backend, error)) {
            errorIfNeeded(error);
        }
        retval << ArrayOf::characterArray(backend);
        return retval;
    }

    if (command == L"isOpen") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 1, 1);
        retval << ArrayOf::logicalScalar(IsWebViewerOpen(getViewerIdArgument(argIn, 1)));
        return retval;
    }

    Error(L"Nelson:webview:unsupportedCommand", L"Unsupported HTML viewer command.");
    return retval;
}
//=============================================================================
