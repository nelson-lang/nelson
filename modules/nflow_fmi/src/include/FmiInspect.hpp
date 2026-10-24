//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsNflow_fmi_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    namespace Fmi {
        //=============================================================================
        // Inspect an FMI 3.0 FMU (a .fmu archive or an already-extracted directory)
        // and return its model description as a JSON object string:
        //
        //   {
        //     "success": true,
        //     "modelIdentifier": "...", "fmiVersion": "3.0",
        //     "coSimulation": true, "modelExchange": false, "scheduledExecution": false,
        //     "variables": [ { "name": "...", "valueReference": 0,
        //                      "causality": "output", "dataType": "Float64" }, ... ]
        //   }
        //
        // On failure returns { "success": false, "error": "..." } and fills
        // errorMessage. A .fmu archive is extracted with the ZIP-slip-hardened unzip
        // into a temporary directory that is removed before returning.
        //=============================================================================
        NLSNFLOW_FMI_IMPEXP std::string
        inspectFmuJson(const std::string& fmuPath, std::string& errorMessage);
        //=============================================================================
    } // namespace Fmi
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
