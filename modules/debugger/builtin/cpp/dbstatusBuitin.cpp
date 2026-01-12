//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>

#include "dbstatusBuiltin.hpp"
#include "Error.hpp"
#include "FileSystemWrapper.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstatusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);

    std::vector<Breakpoint> breakpoints = eval->getBreakpoints();

    // ---------------------------------------------------------------------
    // Group breakpoints by (file, effective function name)
    // ---------------------------------------------------------------------
    struct BPGroup
    {
        std::wstring file;
        std::string name;
        std::vector<double> lines;
    };

    std::vector<BPGroup> groups;

    for (const auto& bp : breakpoints) {
        if (bp.stepMode || bp.line == 0) {
            continue;
        }

        // Canonical function name
        std::string functionFilename = FileSystemWrapper::Path(bp.filename).stem().string();
        std::string effectiveName;
        if (bp.functionName.empty()) {
            effectiveName = functionFilename;
        } else {
            if (bp.functionName != functionFilename) {
                effectiveName = functionFilename + ">" + bp.functionName;
            } else {
                effectiveName = bp.functionName;
            }
        }

        auto it = std::find_if(groups.begin(), groups.end(),
            [&](const BPGroup& g) { return g.file == bp.filename && g.name == effectiveName; });

        if (it == groups.end()) {
            BPGroup g;
            g.file = bp.filename;
            g.name = effectiveName;
            g.lines.push_back(static_cast<double>(bp.line));
            groups.push_back(std::move(g));
        } else {
            it->lines.push_back(static_cast<double>(bp.line));
        }
    }

    // Sort lines inside each group
    for (auto& g : groups) {
        std::sort(g.lines.begin(), g.lines.end());
    }

    // ---------------------------------------------------------------------
    // Display mode
    // ---------------------------------------------------------------------
    if (nLhs == 0) {
        Interface* io = eval->getInterface();

        for (const auto& g : groups) {
            std::wstring formatmsg;
            if (g.lines.size() == 1) {
                formatmsg = _W("Breakpoint for %s is on line: %s");
            } else {
                formatmsg = _W("Breakpoints for %s are on lines: %s");
            }

            std::wstring linesStr;
            for (size_t i = 0; i < g.lines.size(); ++i) {
                linesStr += std::to_wstring(static_cast<size_t>(g.lines[i]));
                if (i + 1 < g.lines.size()) {
                    linesStr += L", ";
                }
            }

            std::wstring msg = fmt::sprintf(formatmsg, utf8_to_wstring(g.name), linesStr);

            io->outputMessage(msg + L"\n");
        }
        return retval;
    }
    // ---------------------------------------------------------------------
    // Structured output mode
    // ---------------------------------------------------------------------
    Dimensions dims;
    dims[0] = groups.size();
    dims[1] = 1;

    ArrayOf structArray;
    if (dims[0] == 0) {
        structArray = ArrayOf::emptyConstructor();
    } else {
        stringVector structNames;
        structNames.push_back("name");
        structNames.push_back("file");
        structNames.push_back("line");

        ArrayOf* rawStruct = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), structNames, false);

        structArray = ArrayOf(NLS_STRUCT_ARRAY, dims, rawStruct, false, structNames);

        ArrayOfVector names;
        ArrayOfVector files;
        ArrayOfVector lines;

        names.reserve(dims.getElementCount());
        files.reserve(dims.getElementCount());
        lines.reserve(dims.getElementCount());

        for (const auto& g : groups) {
            names << ArrayOf::characterArrayConstructor(g.name);
            files << ArrayOf::characterArrayConstructor(g.file);
            lines << ArrayOf::doubleRowVectorConstructor(g.lines);
        }

        structArray.setFieldAsList("name", names);
        structArray.setFieldAsList("file", files);
        structArray.setFieldAsList("line", lines);
    }

    retval.push_back(structArray);
    return retval;
}
//=============================================================================
