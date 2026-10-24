//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "terminalBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Terminal.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::wstring
getTextArgument(const ArrayOf& value, const std::wstring& name)
{
    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray())) {
        Error(L"Nelson:validators:mustBeTextScalar",
            L"%s must be a character vector or string scalar.", name);
    }
    return value.asWideString();
}
//=============================================================================
void
readOption(const ArrayOf& options, const std::string& fieldName, std::wstring& destination)
{
    if (options.isField(fieldName)) {
        destination = getTextArgument(
            options.field(fieldName), std::wstring(fieldName.begin(), fieldName.end()));
    }
}
//=============================================================================
Terminal::TerminalOptions
parseOptions(const ArrayOf& options)
{
    if (!options.isStruct() || !options.isScalar()) {
        Error(L"Nelson:terminal:invalidOptions", L"Terminal options must be a scalar structure.");
    }
    Terminal::TerminalOptions parsed;
    readOption(options, "Name", parsed.name);
    readOption(options, "Shell", parsed.shell);
    readOption(options, "WindowStyle", parsed.windowStyle);
    readOption(options, "Theme", parsed.theme);
    readOption(options, "StartupCommand", parsed.startupCommand);
    if (parsed.windowStyle.empty()) {
        parsed.windowStyle = L"docked";
    }
    if (parsed.theme.empty()) {
        parsed.theme = L"auto";
    }
    return parsed;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::GuiGateway::terminalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    std::wstring action = getTextArgument(argIn[0], L"Action");
    ArrayOfVector retval;
    if (action == L"create") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        retval << Terminal::create(parseOptions(argIn[1]));
        return retval;
    }
    if (action == L"write") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 0, 0);
        Terminal::write(argIn[1], getTextArgument(argIn[2], L"Text"));
        return retval;
    }
    if (action == L"read") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArray(Terminal::read(argIn[1]));
        return retval;
    }
    if (action == L"close") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 0);
        Terminal::close(eval, argIn[1]);
        return retval;
    }
    if (action == L"focus") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 0);
        Terminal::focus(argIn[1]);
        return retval;
    }
    if (action == L"clear") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 0);
        Terminal::clear(argIn[1]);
        return retval;
    }
    if (action == L"get") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 0, 1);
        retval << Terminal::get(argIn[1], getTextArgument(argIn[2], L"PropertyName"));
        return retval;
    }
    if (action == L"set") {
        nargincheck(argIn, 4, 4);
        nargoutcheck(nLhs, 0, 0);
        Terminal::set(argIn[1], getTextArgument(argIn[2], L"PropertyName"), argIn[3]);
        return retval;
    }
    if (action == L"list") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << Terminal::list();
        return retval;
    }
    if (action == L"closeAll") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 0);
        Terminal::closeAll(eval);
        return retval;
    }
    if (action == L"themes") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(Terminal::themes());
        return retval;
    }
    if (action == L"version") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArray(Terminal::version());
        return retval;
    }
    Error(L"Nelson:terminal:unsupportedAction", L"Unsupported terminal action.");
    return retval;
}
//=============================================================================
