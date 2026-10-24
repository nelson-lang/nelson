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
#include <functional>
#include <memory>
#include <string>
#include <vector>
#include "SimEngineTypes.hpp" // IBlockResource, SimDiagnostic
#include "nlsNflow_blocks_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    // Seam for embedding an FMI Co-Simulation component as an NFlow block.
    // nflow_blocks defines the interface; nflow_fmi implements it and registers
    // a factory at startup, so nflow_blocks has no FMI / fmi4c dependency.
    //
    // An IFmuComponent is owned by BlockState::external: the FMU instance (and
    // any temporary extraction directory) is released when the simulation ends,
    // fails, or is cancelled.
    //=============================================================================
    class IFmuComponent : public IBlockResource
    {
    public:
        // Instantiate and run initialization mode for [startTime, stopTime].
        // Named parameters (tunable start values of any numeric/boolean FMI type,
        // converted from double) are applied in initialization mode. Called once
        // from the block INIT phase.
        virtual bool
        initialize(double startTime, double stopTime,
            const std::vector<std::string>& parameterNames,
            const std::vector<double>& parameterValues, SimDiagnostic& diag)
            = 0;

        // Number of portable (numeric/boolean, double-backed) input / output
        // PORTS exposed by the FMU. One port per portable variable; an
        // array-valued FMI3 variable is a single port of width > 1.
        virtual int
        numInputs() const
            = 0;
        virtual int
        numOutputs() const
            = 0;

        // Element count (array size) of input / output port p; 1 for a scalar.
        virtual int
        inputPortWidth(int) const
        {
            return 1;
        }
        virtual int
        outputPortWidth(int) const
        {
            return 1;
        }

        virtual int
        totalInputElements() const
        {
            int t = 0;
            for (int p = 0; p < numInputs(); ++p) {
                t += inputPortWidth(p);
            }
            return t;
        }
        virtual int
        totalOutputElements() const
        {
            int t = 0;
            for (int p = 0; p < numOutputs(); ++p) {
                t += outputPortWidth(p);
            }
            return t;
        }

        // Set the inputs from a flat array of `n` elements spanning every input
        // port in declaration order (port p occupies inputPortWidth(p) slots).
        // Each value is converted to the variable's FMI type.
        virtual bool
        setInputs(const double* values, int n, SimDiagnostic& diag)
            = 0;

        // Copy the current outputs (converted to double) into a flat array of
        // `n` elements spanning every output port. Pure query of FMU state.
        virtual bool
        getOutputs(double* out, int n, SimDiagnostic& diag)
            = 0;

        virtual bool
        doStep(double currentTime, double stepSize, SimDiagnostic& diag)
            = 0;

        // Optional: the diagram's relative tolerance, set before initialize() and
        // passed to the FMU in enterInitializationMode. Ignored by default.
        virtual void
        setSolverTolerance(double relTol)
        {
            (void)relTol;
        }

        // Optional: snapshot / rollback of the full FMU state, used by iterative
        // co-simulation coupling and step rejection. canSaveState() reports
        // canGetAndSetFMUState; restoreState() rolls back to the last saveState().
        // The defaults report "unsupported".
        virtual bool
        canSaveState() const
        {
            return false;
        }
        virtual bool
        saveState(SimDiagnostic&)
        {
            return false;
        }
        virtual bool
        restoreState(SimDiagnostic&)
        {
            return false;
        }

        // Optional: restrict and order the exposed ports to the diagram's declared
        // variable names, so they line up with the wired signals instead of every
        // FMU variable in modelDescription order. The default keeps all ports.
        // Called before initialize().
        virtual bool
        configurePorts(const std::vector<std::string>& inputNames,
            const std::vector<std::string>& outputNames, SimDiagnostic& diag)
        {
            (void)inputNames;
            (void)outputNames;
            (void)diag;
            return true;
        }
    };
    //=============================================================================
    // Factory: takes an FMU path (a .fmu archive or an extracted directory) and
    // returns a component ready to initialize, or nullptr with a diagnostic.
    //=============================================================================
    using FmuComponentFactory
        = std::function<std::unique_ptr<IFmuComponent>(const std::string& fmuPath, SimDiagnostic&)>;
    NLSNFLOW_BLOCKS_IMPEXP void
    registerFmuComponentFactory(FmuComponentFactory factory);
    // True when nflow_fmi is loaded and has registered a factory.
    NLSNFLOW_BLOCKS_IMPEXP bool
    hasFmuComponentFactory();
    // Fills diag and returns nullptr when no factory is registered or the FMU
    // cannot be loaded.
    NLSNFLOW_BLOCKS_IMPEXP std::unique_ptr<IFmuComponent>
    makeFmuComponent(const std::string& fmuPath, SimDiagnostic& diag);
    //=============================================================================
    // Seam for embedding an FMI 3.0 Model Exchange component as an NFlow block.
    // The FMU exposes only the model equations (derivatives, outputs, event
    // indicators); the NFlow solver owns the integration. The block joins the
    // global continuous-state vector and its event indicators feed the shared
    // zero-crossing machinery.
    //
    // Lifecycle:
    //   initialize()   once, from the block INIT phase (instantiate, init, and
    //                  enter continuous-time mode; capture the initial states).
    //   setStateTime() + getOutputs()      per OUTPUT evaluation.
    //   setStateTime() + getDerivatives()  per DERIVATIVE evaluation (RHS).
    //   setStateTime() + getEventIndicators() per ZERO_CROSSING evaluation.
    //   handleEvent()  at a located crossing (enter event mode, update discrete
    //                  states, read back the reset continuous states).
    //=============================================================================
    class IFmuMeComponent : public IBlockResource
    {
    public:
        // Initialization mode, tunable parameters (as for Co-Simulation), then
        // continuous-time mode with the initial continuous states captured.
        // Called once from the block INIT phase.
        virtual bool
        initialize(double startTime, double stopTime,
            const std::vector<std::string>& parameterNames,
            const std::vector<double>& parameterValues, SimDiagnostic& diag)
            = 0;

        // Optional: the diagram's relative tolerance, set before initialize() and
        // passed to the FMU in enterInitializationMode. Ignored by default.
        virtual void
        setSolverTolerance(double relTol)
        {
            (void)relTol;
        }

        // Known after initialize().
        virtual int
        numContinuousStates() const
            = 0;
        virtual int
        numEventIndicators() const
            = 0;

        // Copies min(n, numContinuousStates()) initial states into x, to seed the
        // global x0.
        virtual void
        getInitialStates(double* x, int n) const
            = 0;

        // Port mapping, same semantics as IFmuComponent.
        virtual int
        numInputs() const
            = 0;
        virtual int
        numOutputs() const
            = 0;
        virtual int
        inputPortWidth(int) const
        {
            return 1;
        }
        virtual int
        outputPortWidth(int) const
        {
            return 1;
        }
        virtual int
        totalInputElements() const
        {
            int t = 0;
            for (int p = 0; p < numInputs(); ++p) {
                t += inputPortWidth(p);
            }
            return t;
        }
        virtual int
        totalOutputElements() const
        {
            int t = 0;
            for (int p = 0; p < numOutputs(); ++p) {
                t += outputPortWidth(p);
            }
            return t;
        }
        virtual bool
        setInputs(const double* values, int n, SimDiagnostic& diag)
            = 0;
        virtual bool
        getOutputs(double* out, int n, SimDiagnostic& diag)
            = 0;

        // setTime + setContinuousStates from the solver. Called before any get*.
        virtual bool
        setStateTime(double time, const double* x, int n, SimDiagnostic& diag)
            = 0;

        virtual bool
        getDerivatives(double* xdot, int n, SimDiagnostic& diag)
            = 0;

        virtual bool
        getEventIndicators(double* g, int n, SimDiagnostic& diag)
            = 0;

        // Processes a located state event: enter event mode, iterate the discrete
        // states to a fixed point, read back the continuous states if reset.
        // xin is the crossing state, xout the post-event state (n elements);
        // statesChanged reports a reset. Leaves the FMU in continuous-time mode.
        virtual bool
        handleEvent(
            const double* xin, double* xout, int n, bool& statesChanged, SimDiagnostic& diag)
            = 0;
    };
    //=============================================================================
    using FmuMeComponentFactory = std::function<std::unique_ptr<IFmuMeComponent>(
        const std::string& fmuPath, SimDiagnostic&)>;
    NLSNFLOW_BLOCKS_IMPEXP void
    registerFmuMeComponentFactory(FmuMeComponentFactory factory);
    NLSNFLOW_BLOCKS_IMPEXP bool
    hasFmuMeComponentFactory();
    NLSNFLOW_BLOCKS_IMPEXP std::unique_ptr<IFmuMeComponent>
    makeFmuMeComponent(const std::string& fmuPath, SimDiagnostic& diag);
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
