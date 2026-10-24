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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "importBuiltin.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
isStringArgument(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
void
printImports(const stringVector& imports)
{
    for (const auto& importSpec : imports) {
        NelsonPrint(utf8_to_wstring(importSpec) + L"\n");
    }
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::importBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    Context* context = eval->getContext();
    if (argIn.empty()) {
        stringVector imports = context->getImports();
        if (nLhs == 0) {
            printImports(imports);
        } else {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(imports);
        }
        return retval;
    }
    for (size_t k = 0; k < argIn.size(); ++k) {
        if (!isStringArgument(argIn[k])) {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (k + 1),
                L"string");
        }
        context->addImport(argIn[k].asUtf8String());
    }
    if (nLhs == 1) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(context->getImports());
    }
    return retval;
}
//=============================================================================
