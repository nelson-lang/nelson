//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_highlightBuiltin.hpp"
#include "nflow_guiBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <atomic>
#include <nlohmann/json.hpp>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// nflow_highlight(block)
//   Flash a block in every open editor window. The designator is 'model/Block'
//   (nested paths allowed). A monotonic stamp makes a repeated call re-trigger
//   the flash. No effect when no editor is open.
//=============================================================================
ArrayOfVector
nflow_highlightBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (!argIn[0].isRowVectorCharacterArray() && !argIn[0].isScalarStringArray()) {
        Error(L"Nelson:nflow_gui:invalidBlockReference",
            L"nflow_highlight: block must be a 'model/Block' string.");
    }
    static std::atomic<unsigned long long> s_stamp { 1 };
    nlohmann::json payload;
    payload["block"] = argIn[0].asUtf8String();
    payload["stamp"] = s_stamp.fetch_add(1);
    broadcastNFlowPush("nflow.highlight", payload.dump());
    return ArrayOfVector();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
