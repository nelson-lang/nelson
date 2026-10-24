//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "new_systemBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <cctype>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static bool
isValidModelName(const std::string& name);
//=============================================================================
ArrayOfVector
new_systemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string name;
    if (argIn.size() >= 1) {
        name = argIn[0].asUtf8String();
        if (!isValidModelName(name)) {
            Error("Nelson:nflow_engine:invalidModelName",
                "Model name must be a valid identifier: %s", name);
        }
    }
    double handle = 0;
    std::string outName, err;
    if (!NFlowModelRegistry::instance().createEmpty(name, handle, outName, err)) {
        Error("Nelson:nflow_engine:newSystemFailed", err);
    }
    NFlowModelRegistry::instance().setCurrentSystem(outName);
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(handle);
    return retval;
}
//=============================================================================
static bool
isValidModelName(const std::string& name)
{
    if (name.empty() || (std::isdigit((unsigned char)name[0]) != 0)) {
        return false;
    }
    for (char c : name) {
        if ((std::isalnum((unsigned char)c) == 0) && c != '_') {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
