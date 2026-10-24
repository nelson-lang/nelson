//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Bridge between block handlers and the Nelson interpreter/workspace.
//
// nlsNflow_blocks links only nlsCommons, so this file merely stores callbacks;
// the nflow_engine builtin layer (the only nflow module linking the
// interpreter) installs implementations that capture the Evaluator and, when
// invoked from a background simulation thread, marshal the work to the
// interpreter thread. When no callbacks are installed (e.g. headless code
// generation), blocks that need them must fail with a clear diagnostic.
//=============================================================================
#pragma once
//=============================================================================
#include <functional>
#include <string>
#include <vector>
#include "nlsNflow_blocks_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    struct NelsonCallResult
    {
        bool ok = false;
        std::string errorMessage;
        std::vector<double> y;
        // Imaginary parts when the function returned a complex result
        // (parallel to y; empty for real results).
        std::vector<double> yImag;
        bool isComplex = false;
    };
    //=============================================================================
    struct WorkspaceSignal
    {
        bool ok = false;
        std::string errorMessage;
        std::vector<double> times;
        // column-major samples: values[k * width + i] = element i at time k
        std::vector<double> values;
        int width = 0;
        // Imaginary parts for a complex signal source (parallel to values).
        std::vector<double> valuesImag;
        bool isComplex = false;
    };
    //=============================================================================
    struct WorkspacePayload
    {
        std::string variableName;
        std::string saveFormat; // "Structure With Time" | "Structure" | "Array"
        std::string blockName;
        std::vector<double> times;
        // column-major samples, same layout as WorkspaceSignal::values
        std::vector<double> values;
        int width = 1;
        // Imaginary parts (parallel to values) for a complex signal.
        std::vector<double> valuesImag;
        bool isComplex = false;
        // Exact 64-bit samples (parallel to values, uint64 as raw bits)
        // written as an exact integer variable when exactType is
        // "int64" / "uint64".
        std::vector<long long> valuesI64;
        std::string exactType;
    };
    //=============================================================================
    // fcnSpec is the block's Fcn parameter: a function name ("sin"), an
    // anonymous function source ("@(u) 2*u"), or an expression using u.
    // uImag (nullable) carries the imaginary parts of a complex input.
    using NelsonCallFn = std::function<NelsonCallResult(const std::string& fcnSpec,
        const std::vector<double>& u, const std::vector<double>* uImag)>;
    using WorkspaceReadFn = std::function<WorkspaceSignal(const std::string& variableName)>;
    using WorkspaceWriteFn = std::function<bool(const WorkspacePayload&, std::string&)>;
    NLSNFLOW_BLOCKS_IMPEXP void
    setNelsonWorkspaceBridge(
        const NelsonCallFn& call, const WorkspaceReadFn& read, const WorkspaceWriteFn& write);
    NLSNFLOW_BLOCKS_IMPEXP bool
    hasNelsonWorkspaceBridge();
    NLSNFLOW_BLOCKS_IMPEXP NelsonCallResult
    callNelsonFunction(const std::string& fcnSpec, const std::vector<double>& u,
        const std::vector<double>* uImag = nullptr);
    NLSNFLOW_BLOCKS_IMPEXP WorkspaceSignal
    readWorkspaceSignal(const std::string& variableName);
    NLSNFLOW_BLOCKS_IMPEXP bool
    writeWorkspaceVariable(const WorkspacePayload& payload, std::string& errorMessage);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
