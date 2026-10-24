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
//=============================================================================
// Structured diagnostic channel, split out of SimEngineTypes.hpp so the
// solver-backend contract (SolverBackend.hpp) can be shared with a low-level
// numeric module (ode_solvers) WITHOUT dragging in the whole simulation engine
// header (SimCtx, block state, nlohmann/json). SimEngineTypes.hpp includes this
// header, so every existing user keeps SimDiagnostic / SimDiagnostics.
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Structured diagnostic channel.
    //
    // A block handler reports a warning or a fatal error through SimCtx::diag
    // instead of the engine hand-formatting JSON error strings. This does NOT
    // change the BlockHandlerFn bool return, which keeps its "output changed"
    // meaning used by the algebraic-loop convergence check.
    struct SimDiagnostic
    {
        std::string code; // stable identifier, e.g. "unsupported_block"
        std::string blockId; // user-facing block id, when applicable
        std::string operation; // optional operation label
        std::string message; // human-readable message
        double time = 0.0; // simulation time when raised
        int status = 0; // optional backend status code
        bool fatal = false;
        // An FMU that returns terminateRequested from doStep asks for a clean
        // early stop (not an error): the engine ends the run and keeps results.
        bool requestStop = false;
        std::vector<std::string> logs;
    };
    //=============================================================================
    class SimDiagnostics
    {
    public:
        void
        warn(SimDiagnostic d)
        {
            d.fatal = false;
            items_.push_back(std::move(d));
        }
        void
        fail(SimDiagnostic d)
        {
            d.fatal = true;
            aborted_ = true;
            items_.push_back(std::move(d));
        }
        bool
        aborted() const
        {
            return aborted_;
        }
        bool
        empty() const
        {
            return items_.empty();
        }
        const std::vector<SimDiagnostic>&
        all() const
        {
            return items_;
        }

    private:
        std::vector<SimDiagnostic> items_;
        bool aborted_ = false;
    };
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
