//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "dbstatusBuiltin.hpp"
#include "BreakpointTarget.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileSystemWrapper.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::DebuggerGateway;
//=============================================================================
namespace {
//=============================================================================
struct BreakpointGroup
{
    std::wstring file;
    std::string name;
    std::vector<double> lines;
    wstringVector conditions;
};
//=============================================================================
std::string
effectiveName(const Breakpoint& bp)
{
    std::string functionFilename = FileSystemWrapper::Path(bp.filename).stem().string();
    if (bp.functionName.empty() || bp.functionName == functionFilename) {
        return functionFilename;
    }
    return functionFilename + ">" + bp.functionName;
}
//=============================================================================
std::wstring
joinLines(const std::vector<double>& lines)
{
    std::wstring text;
    for (size_t i = 0; i < lines.size(); ++i) {
        text += std::to_wstring(static_cast<size_t>(lines[i]));
        if (i + 1 < lines.size()) {
            text += L", ";
        }
    }
    return text;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstatusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // dbstatus [file] [-completenames], b = dbstatus(...)
    ArrayOfVector retval;
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    bool completeNames = false;
    std::wstring fileFilter;
    for (const std::wstring& token : breakpointCommandTokens(argIn)) {
        if (token == L"-completenames") {
            completeNames = true;
        } else if (fileFilter.empty()) {
            fileFilter = token;
        } else {
            Error(L"Nelson:dbstatus:invalidArgument", L"Invalid dbstatus syntax.");
        }
    }
    std::wstring filterFilename;
    if (!fileFilter.empty()) {
        BreakpointTarget target;
        std::wstring errorMessage;
        if (!resolveBreakpointTarget(eval, fileFilter, target, errorMessage)) {
            RaiseError(errorMessage, L"Nelson:dbstatus:functionNotFound");
        }
        filterFilename = target.filename;
    }
    std::vector<BreakpointGroup> groups;
    for (const Breakpoint& bp : eval->getUserBreakpoints()) {
        if (bp.line == 0) {
            continue;
        }
        if (!filterFilename.empty() && !sameBreakpointFile(bp.filename, filterFilename)) {
            continue;
        }
        std::string name = effectiveName(bp);
        auto it = std::find_if(groups.begin(), groups.end(),
            [&](const BreakpointGroup& g) { return g.file == bp.filename && g.name == name; });
        if (it == groups.end()) {
            BreakpointGroup group;
            group.file = bp.filename;
            group.name = name;
            groups.push_back(std::move(group));
            it = groups.end() - 1;
        }
        // getUserBreakpoints is sorted by line: insertion keeps the order
        it->lines.push_back(static_cast<double>(bp.line));
        it->conditions.push_back(bp.condition);
    }
    // error breakpoints are global: not listed when a file is given
    bool listErrors = filterFilename.empty();
    bool stopOnError = listErrors && eval->getStopOnError();
    bool stopOnCaughtError = listErrors && eval->getStopOnCaughtError();
    auto displayName = [&](const BreakpointGroup& g) -> std::wstring {
        std::wstring name = utf8_to_wstring(g.name);
        if (completeNames) {
            return g.file + L">" + name;
        }
        return name;
    };
    if (nLhs == 0) {
        Interface* io = eval->getInterface();
        for (const auto& g : groups) {
            std::wstring message;
            if (g.lines.size() == 1) {
                message = formatI18nMessage(
                    _W("Breakpoint for %s is on line %s."), displayName(g), joinLines(g.lines));
            } else {
                message = formatI18nMessage(
                    _W("Breakpoints for %s are on lines %s."), displayName(g), joinLines(g.lines));
            }
            io->outputMessage(message + L"\n");
            for (size_t k = 0; k < g.lines.size(); ++k) {
                if (!g.conditions[k].empty()) {
                    io->outputMessage(
                        formatI18nMessage(
                            _W("  Breakpoint on line %s has conditional expression %s."),
                            std::to_wstring(static_cast<size_t>(g.lines[k])),
                            L"\"" + g.conditions[k] + L"\"")
                        + L"\n");
                }
            }
        }
        if (stopOnError || stopOnCaughtError) {
            if (!groups.empty()) {
                io->outputMessage(L"\n");
            }
            if (stopOnError) {
                io->outputMessage(_W("Stop if error.") + L"\n");
            }
            if (stopOnCaughtError) {
                io->outputMessage(_W("Stop if caught error.") + L"\n");
            }
        }
        return retval;
    }
    stringVector fieldNames
        = { "name", "file", "line", "anonymous", "expression", "cond", "identifier" };
    size_t count = groups.size() + (stopOnError ? 1 : 0) + (stopOnCaughtError ? 1 : 0);
    Dimensions dims(count, 1);
    if (count == 0) {
        retval.push_back(ArrayOf::empty());
        return retval;
    }
    auto* rawStruct = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldNames, false));
    ArrayOf structArray(NLS_STRUCT_ARRAY, dims, rawStruct, false, fieldNames);
    ArrayOfVector names;
    ArrayOfVector files;
    ArrayOfVector lines;
    ArrayOfVector anonymous;
    ArrayOfVector expressions;
    ArrayOfVector conds;
    ArrayOfVector identifiers;
    for (const auto& g : groups) {
        names << ArrayOf::characterArray(displayName(g));
        files << ArrayOf::characterArray(g.file);
        lines << ArrayOf::doubleRowVector(g.lines);
        anonymous << ArrayOf::doubleRowVector(std::vector<double>(g.lines.size(), 0.0));
        expressions << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(g.conditions);
        conds << ArrayOf::characterArray(L"");
        Dimensions emptyDims(0, 0);
        identifiers << ArrayOf::emptyCell(emptyDims);
    }
    auto addErrorEntry = [&](const std::wstring& cond) {
        names << ArrayOf::characterArray(L"");
        files << ArrayOf::characterArray(L"");
        lines << ArrayOf::empty();
        anonymous << ArrayOf::empty();
        Dimensions emptyDims(0, 0);
        expressions << ArrayOf::emptyCell(emptyDims);
        conds << ArrayOf::characterArray(cond);
        identifiers << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(wstringVector { L"all" });
    };
    if (stopOnError) {
        addErrorEntry(L"error");
    }
    if (stopOnCaughtError) {
        addErrorEntry(L"caught error");
    }
    structArray.setFieldAsList("name", names);
    structArray.setFieldAsList("file", files);
    structArray.setFieldAsList("line", lines);
    structArray.setFieldAsList("anonymous", anonymous);
    structArray.setFieldAsList("expression", expressions);
    structArray.setFieldAsList("cond", conds);
    structArray.setFieldAsList("identifier", identifiers);
    retval.push_back(structArray);
    return retval;
}
//=============================================================================
