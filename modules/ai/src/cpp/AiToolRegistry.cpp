//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AiToolRegistry.hpp"
//=============================================================================
namespace Nelson::AiMcp {
//=============================================================================
std::optional<ToolResult>
invokeTool(
    Evaluator* eval, const std::wstring& name, const ArrayOf& args, const McpOptions& options)
{
    if (name == L"detect_nelson_modules") {
        return detectNelsonModules(args);
    }
    if (name == L"check_nelson_code") {
        return checkNelsonCode(eval, args, options);
    }
    if (name == L"lint_nelson_code") {
        return lintNelsonCode(eval, args, options);
    }
    if (name == L"check_nelson_format") {
        return checkNelsonFormat(eval, args, options);
    }
    if (name == L"evaluate_nelson_code") {
        return evaluateNelsonCode(eval, args, options);
    }
    if (name == L"run_nelson_file") {
        return runNelsonFile(eval, args, options);
    }
    if (name == L"run_nelson_test_file") {
        return runNelsonTest(eval, args, options);
    }
    if (name == L"format_nelson_file") {
        return formatNelsonFile(eval, args, options);
    }
    if (name == L"get_nelson_help") {
        return getNelsonHelp(eval, args);
    }
    if (name == L"find_nelson_symbol") {
        return findNelsonSymbol(args);
    }
    if (name == L"get_nelson_function_info") {
        return getNelsonFunctionInfo(eval, args, options);
    }
    if (name == L"get_nelson_version") {
        return getNelsonVersion(eval, options);
    }
    if (name == L"get_nelson_workspace") {
        return getNelsonWorkspace(options);
    }
    if (name == L"create_nelson_plot") {
        return createNelsonPlot(eval, args, options);
    }
    if (name == L"open_in_nelson_editor") {
        return openInNelsonEditor(eval, args, options);
    }
    if (name == L"capture_nelson_figure") {
        return captureNelsonFigure(eval, args, options);
    }
    return std::nullopt;
}
//=============================================================================
} // namespace Nelson::AiMcp
//=============================================================================
