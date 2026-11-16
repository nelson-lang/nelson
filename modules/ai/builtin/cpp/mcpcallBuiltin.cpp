//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mcpcallBuiltin.hpp"
#include "AiToolRegistry.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AiGateway::mcpcallBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isRowVectorCharacterArray() && !argIn[0].isScalarStringArray()) {
        Error(L"Nelson:ai:toolNameExpected", L"%s expected.", L"a tool name");
    }
    std::wstring name = argIn[0].asWideString();
    ArrayOf args;
    if (argIn.size() == 2) {
        if (!argIn[1].isStruct() || !argIn[1].isScalar()) {
            Error(
                L"Nelson:ai:argsStructExpected", L"%s expected.", L"a scalar struct of arguments");
        }
        args = argIn[1];
    } else {
        args = ArrayOf::structArray(stringVector {}, ArrayOfVector {});
    }
    AiMcp::McpOptions options;
    std::optional<AiMcp::ToolResult> result = AiMcp::invokeTool(eval, name, args, options);
    if (!result.has_value()) {
        Error(L"Nelson:ai:unknownTool", L"Unknown tool: %s.", name);
    }
    const AiMcp::ToolResult& toolResult = result.value();
    stringVector fieldNames = { "success", "output", "error", "image", "data", "duration" };
    ArrayOfVector values;
    values.push_back(ArrayOf::logicalScalar(toolResult.success));
    values.push_back(ArrayOf::characterArray(toolResult.text));
    values.push_back(ArrayOf::characterArray(toolResult.error));
    values.push_back(ArrayOf::characterArray(toolResult.image));
    values.push_back(ArrayOf::characterArray(toolResult.dataJson));
    values.push_back(ArrayOf::doubleScalar(toolResult.duration));
    return ArrayOfVector { ArrayOf::structScalar(fieldNames, values) };
}
//=============================================================================
