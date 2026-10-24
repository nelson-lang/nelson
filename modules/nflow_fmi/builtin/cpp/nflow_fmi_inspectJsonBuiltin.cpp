//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_fmi_inspectJsonBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FmiInspect.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __nflow_fmi_inspect_json__(fmuPathOrDir) -> char row vector (JSON)
//
// Returns the FMU model description as a JSON string, the exact payload served
// by the GUI nflow.fmi3.inspect RPC. Thin wrapper over the library helper
// Nelson::NFlow::Fmi::inspectFmuJson so the RPC path is testable headlessly.
//=============================================================================
ArrayOfVector
Nelson::NFlowFmiGateway::nflow_fmi_inspectJsonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    const std::string path = wstring_to_utf8(argIn[0].asWideString());
    std::string errorMessage;
    const std::string json = Nelson::NFlow::Fmi::inspectFmuJson(path, errorMessage);
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(utf8_to_wstring(json));
    return retval;
}
//=============================================================================
