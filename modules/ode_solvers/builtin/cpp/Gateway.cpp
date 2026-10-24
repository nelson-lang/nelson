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
#include "odeBvpKinsolBuiltin.hpp"
#include "odeDevalInterpolateBuiltin.hpp"
#include "odeErrorNormBuiltin.hpp"
#include "odeSundialsAvailabilityBuiltin.hpp"
#include "odeSundialsCapabilitiesBuiltin.hpp"
#include "odeSundialsRunBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"ode_solvers";
//=============================================================================
static const nlsGateway gateway[]
    = { { "__ode_error_norm__", (ptrBuiltin)Nelson::OdeSolversGateway::odeErrorNormBuiltin, 1, 6 },
          { "__ode_deval_interpolate__",
              (ptrBuiltin)Nelson::OdeSolversGateway::odeDevalInterpolateBuiltin, 2, 4 },
          { "__ode_sundials_available__",
              (ptrBuiltin)Nelson::OdeSolversGateway::odeSundialsAvailabilityBuiltin, 1, 0 },
          { "__ode_sundials_capabilities__",
              (ptrBuiltin)Nelson::OdeSolversGateway::odeSundialsCapabilitiesBuiltin, 1, 0 },
          { "__ode_sundials_run__", (ptrBuiltin)Nelson::OdeSolversGateway::odeSundialsRunBuiltin, 1,
              1, CPP_BUILTIN_WITH_EVALUATOR },
          { "__bvp_kinsol_solve__", (ptrBuiltin)Nelson::OdeSolversGateway::odeBvpKinsolBuiltin, 2,
              3, CPP_BUILTIN_WITH_EVALUATOR } };
//=============================================================================
// The CVODES / IDAS backends live in the ode_solvers core library, so this builtin
// loads in a math-only install (no NFlow, Qt or webview).
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
