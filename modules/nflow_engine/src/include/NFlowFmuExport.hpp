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
#include <vector>
#include "nlsNflow_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // One FMI 3.0 scalar variable of an exported model. `type` is the FMI type
    // element name (Float64 | Int64 | UInt64); `causality` is input | output |
    // independent. `start` is the initial value (required by FMI for inputs and
    // the independent variable; empty for calculated outputs).
    struct FmuExportVar
    {
        std::string name;
        std::string causality;
        std::string type;
        unsigned valueReference = 0;
        std::string start;
    };
    //=============================================================================
    // The I/O interface of a model to export, in the FMI sense. Built from the
    // generated code's input/output name+type tables.
    struct FmuInterfaceVar
    {
        std::string name;
        std::string type; // Float64 | Int64 | UInt64
    };
    //=============================================================================
    struct FmuModelInfo
    {
        std::string modelName;
        std::string instantiationToken; // stable per-model token (a GUID-like string)
        std::string generationTool; // e.g. "Nelson/nflow"
        double stepSize = 0.01;
        double stopTime = 10.0;
        std::vector<FmuExportVar> variables; // time (VR 0), inputs, then outputs
        // Model carries a stopSimulation block: the generated ModelState has a
        // nflow_stop_requested flag and fmi3DoStep must surface it through
        // *terminateSimulation (set by the caller from the generated source).
        bool hasStopFlag = false;
    };
    //=============================================================================
    // Assemble the FMI variable list with deterministic value references:
    // time = 0 (independent), inputs = 1..NI in order, outputs follow. Inputs get
    // start="0"; the independent variable gets start="0" too.
    NLSNFLOW_ENGINE_IMPEXP FmuModelInfo
    buildFmuModelInfo(const std::string& modelName, const std::string& instantiationToken,
        double stepSize, double stopTime, const std::vector<FmuInterfaceVar>& inputs,
        const std::vector<FmuInterfaceVar>& outputs);
    //=============================================================================
    NLSNFLOW_ENGINE_IMPEXP std::string
    buildModelDescriptionXml(const FmuModelInfo& info);
    //=============================================================================
    // Write the FMI 3.0 buildDescription.xml for a source-code FMU: one C99
    // SourceFileSet listing the generated model.c and fmi3Functions.c.
    NLSNFLOW_ENGINE_IMPEXP std::string
    buildBuildDescriptionXml(const std::string& modelIdentifier);
    //=============================================================================
    // FMI 3.0 Co-Simulation wrapper (fmi3Functions.c) that adapts the generated
    // model.c (nflow_init / nflow_step / nflow_terminate) to the FMI 3.0 C API.
    // Assumes a Float64-only interface (the export layer rejects other types).
    NLSNFLOW_ENGINE_IMPEXP std::string
    buildFmi3FunctionsC(const FmuModelInfo& info);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
