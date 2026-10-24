//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "nflow_fmi_availableBuiltin.hpp"
#include "nflow_fmi_inspectBuiltin.hpp"
#include "nflow_fmi_cosimBuiltin.hpp"
#include "nflow_fmi_modelexchangeBuiltin.hpp"
#include "nflow_fmi_inspectJsonBuiltin.hpp"
#include "nflow_fmi_stateRoundtripBuiltin.hpp"
#include "FmuComponent.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"nflow_fmi";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__nflow_fmi_available__", (ptrBuiltin)Nelson::NFlowFmiGateway::nflow_fmi_availableBuiltin, 1,
        0 },
    { "__nflow_fmi_inspect__", (ptrBuiltin)Nelson::NFlowFmiGateway::nflow_fmi_inspectBuiltin, 1,
        1 },
    { "__nflow_fmi_cosim__", (ptrBuiltin)Nelson::NFlowFmiGateway::nflow_fmi_cosimBuiltin, 1, 3 },
    { "__nflow_fmi_modelexchange__",
        (ptrBuiltin)Nelson::NFlowFmiGateway::nflow_fmi_modelexchangeBuiltin, 1, 3 },
    { "__nflow_fmi_inspect_json__",
        (ptrBuiltin)Nelson::NFlowFmiGateway::nflow_fmi_inspectJsonBuiltin, 1, 1 },
    { "__nflow_fmi_state_roundtrip__",
        (ptrBuiltin)Nelson::NFlowFmiGateway::nflow_fmi_stateRoundtripBuiltin, 1, 2 },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
static bool
finishNFlowFmiModule(Nelson::Evaluator*)
{
    Nelson::NFlow::registerFmuComponentFactory({});
    Nelson::NFlow::registerFmuMeComponentFactory({});
    return true;
}
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishNFlowFmiModule)
//=============================================================================
