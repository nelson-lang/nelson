//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "OdeCore.hpp"
#ifdef NELSON_WITH_SUNDIALS
//=============================================================================
#include <algorithm>
#include <cmath>
#include <cvodes/cvodes.h>
#include <cvodes/cvodes_ls.h>
#include <idas/idas.h>
#include <idas/idas_ls.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_context.h>
#include <sundials/sundials_types.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunmatrix/sunmatrix_dense.h>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace OdeCore {
    //=============================================================================
    namespace {
        struct CallbackContext
        {
            const RhsFn* rhs = nullptr;
            const RootFn* roots = nullptr;
        };
        //=============================================================================
        int
        rhsTrampoline(sunrealtype t, N_Vector y, N_Vector ydot, void* user)
        {
            auto* c = static_cast<CallbackContext*>(user);
            return (*c->rhs)(static_cast<double>(t), NV_DATA_S(y), NV_DATA_S(ydot)) ? 0 : 1;
        }
        //=============================================================================
        int
        rootTrampoline(sunrealtype t, N_Vector y, sunrealtype* gout, void* user)
        {
            auto* c = static_cast<CallbackContext*>(user);
            return (*c->roots)(static_cast<double>(t), NV_DATA_S(y), gout) ? 0 : 1;
        }
        //=============================================================================
        struct Resources
        {
            SUNContext ctx { nullptr };
            N_Vector y { nullptr };
            N_Vector absTol { nullptr };
            SUNMatrix A { nullptr };
            SUNLinearSolver LS { nullptr };
            void* mem { nullptr };

            ~Resources()
            {
                if (mem != nullptr) {
                    CVodeFree(&mem);
                }
                if (LS != nullptr) {
                    SUNLinSolFree(LS);
                }
                if (A != nullptr) {
                    SUNMatDestroy(A);
                }
                if (absTol != nullptr) {
                    N_VDestroy_Serial(absTol);
                }
                if (y != nullptr) {
                    N_VDestroy_Serial(y);
                }
                if (ctx != nullptr) {
                    SUNContext_Free(&ctx);
                }
            }
        };
        //=============================================================================
        CvodesResult
        fail(CvodesStatus status, const std::string& message)
        {
            CvodesResult r;
            r.status = status;
            r.message = message;
            return r;
        }
        //=============================================================================
        void
        collectStats(void* mem, CvodesStats& s)
        {
            long v = 0;
            if (CVodeGetNumSteps(mem, &v) == CV_SUCCESS) {
                s.steps = v;
            }
            if (CVodeGetNumRhsEvals(mem, &v) == CV_SUCCESS) {
                s.rhsEvals = v;
            }
            if (CVodeGetNumGEvals(mem, &v) == CV_SUCCESS) {
                s.rootEvals = v;
            }
            if (CVodeGetNumErrTestFails(mem, &v) == CV_SUCCESS) {
                s.errTestFails = v;
            }
            if (CVodeGetNumNonlinSolvConvFails(mem, &v) == CV_SUCCESS) {
                s.nonlinConvFails = v;
            }
            if (CVodeGetNumJacEvals(mem, &v) == CV_SUCCESS) {
                s.jacEvals = v;
            }
            sunrealtype h = 0.0;
            if (CVodeGetLastStep(mem, &h) == CV_SUCCESS) {
                s.lastStep = static_cast<double>(h);
            }
        }
        //=============================================================================
        CvodesStatus
        mapFlag(int flag)
        {
            switch (flag) {
            case CV_TOO_MUCH_WORK:
                return CvodesStatus::TooMuchWork;
            case CV_TOO_MUCH_ACC:
            case CV_ERR_FAILURE:
                return CvodesStatus::TolTooSmall;
            case CV_RHSFUNC_FAIL:
            case CV_FIRST_RHSFUNC_ERR:
            case CV_REPTD_RHSFUNC_ERR:
            case CV_UNREC_RHSFUNC_ERR:
                return CvodesStatus::RhsFailure;
            case CV_LSETUP_FAIL:
            case CV_LSOLVE_FAIL:
                return CvodesStatus::LinearSolverFailure;
            default:
                return CvodesStatus::Error;
            }
        }
        //=============================================================================
        // Shared CVODES setup for both runCvodes and CvodesSession.
        bool
        setup(Resources& res, CallbackContext& cb, int n, const std::vector<double>& y0, double t0,
            const CvodesOptions& opts, std::string& err)
        {
            if (SUNContext_Create(SUN_COMM_NULL, &res.ctx) < 0) {
                err = "SUNContext_Create failed";
                return false;
            }
            res.y = N_VNew_Serial(n, res.ctx);
            if (res.y == nullptr) {
                err = "N_VNew_Serial failed";
                return false;
            }
            std::copy(y0.begin(), y0.end(), NV_DATA_S(res.y));
            const int method = (opts.method == "adams") ? CV_ADAMS : CV_BDF;
            res.mem = CVodeCreate(method, res.ctx);
            if (res.mem == nullptr) {
                err = "CVodeCreate failed";
                return false;
            }
            if (CVodeInit(res.mem, rhsTrampoline, t0, res.y) != CV_SUCCESS) {
                err = "CVodeInit failed";
                return false;
            }
            if (opts.absTol.size() > 1) {
                if (static_cast<int>(opts.absTol.size()) != n) {
                    err = "absTol size must be 1 or n";
                    return false;
                }
                res.absTol = N_VNew_Serial(n, res.ctx);
                std::copy(opts.absTol.begin(), opts.absTol.end(), NV_DATA_S(res.absTol));
                CVodeSVtolerances(res.mem, opts.relTol, res.absTol);
            } else {
                CVodeSStolerances(
                    res.mem, opts.relTol, opts.absTol.empty() ? 1e-8 : opts.absTol.front());
            }
            CVodeSetUserData(res.mem, &cb);
            res.A = SUNDenseMatrix(n, n, res.ctx);
            res.LS = SUNLinSol_Dense(res.y, res.A, res.ctx);
            if (res.A == nullptr || res.LS == nullptr
                || CVodeSetLinearSolver(res.mem, res.LS, res.A) != CV_SUCCESS) {
                err = "dense linear solver setup failed";
                return false;
            }
            if (opts.initStep > 0.0) {
                CVodeSetInitStep(res.mem, opts.initStep);
            }
            if (opts.maxStep > 0.0) {
                CVodeSetMaxStep(res.mem, opts.maxStep);
            }
            if (opts.minStep > 0.0) {
                CVodeSetMinStep(res.mem, opts.minStep);
            }
            if (opts.maxOrder > 0) {
                CVodeSetMaxOrd(res.mem, opts.maxOrder);
            }
            if (opts.maxNumSteps > 0) {
                CVodeSetMaxNumSteps(res.mem, opts.maxNumSteps);
            }
            if (opts.numRoots > 0) {
                if (CVodeRootInit(res.mem, opts.numRoots, rootTrampoline) != CV_SUCCESS) {
                    err = "CVodeRootInit failed";
                    return false;
                }
                if (!opts.rootDirection.empty()) {
                    std::vector<int> dir = opts.rootDirection;
                    CVodeSetRootDirection(res.mem, dir.data());
                }
            }
            return true;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    CvodesResult
    runCvodes(const RhsFn& rhs, const RootFn& roots, const std::vector<double>& y0,
        const std::vector<double>& tspan, const CvodesOptions& opts)
    {
        const int n = static_cast<int>(y0.size());
        if (n == 0) {
            return fail(CvodesStatus::Error, "empty state vector");
        }
        if (tspan.size() < 2) {
            return fail(CvodesStatus::Error, "tspan needs at least two points");
        }
        Resources res;
        CallbackContext cb;
        cb.rhs = &rhs;
        cb.roots = &roots;
        std::string err;
        if (!setup(res, cb, n, y0, tspan.front(), opts, err)) {
            return fail(CvodesStatus::Error, err);
        }

        CvodesResult result;
        auto appendRow = [&](double t) {
            result.t.push_back(t);
            const double* yd = NV_DATA_S(res.y);
            result.y.insert(result.y.end(), yd, yd + n);
        };
        appendRow(tspan.front());

        for (size_t k = 1; k < tspan.size(); ++k) {
            sunrealtype tret = tspan.front();
            int flag = CVode(res.mem, tspan[k], res.y, &tret, CV_NORMAL);
            if (flag == CV_ROOT_RETURN) {
                std::vector<int> found(opts.numRoots, 0);
                CVodeGetRootInfo(res.mem, found.data());
                for (int i = 0; i < opts.numRoots; ++i) {
                    if (found[i] != 0) {
                        result.rootsFound.push_back(i);
                    }
                }
                appendRow(static_cast<double>(tret));
                result.status = CvodesStatus::RootFound;
                collectStats(res.mem, result.stats);
                return result;
            }
            if (flag < 0) {
                result.status = mapFlag(flag);
                result.message = "CVode failed with flag " + std::to_string(flag);
                collectStats(res.mem, result.stats);
                return result;
            }
            appendRow(static_cast<double>(tret));
        }
        result.status = CvodesStatus::Success;
        collectStats(res.mem, result.stats);
        return result;
    }
    //=============================================================================
    struct CvodesSession::Impl
    {
        Resources res;
        RhsFn rhs;
        RootFn roots;
        CallbackContext cb;
        int n { 0 };
        int numRoots { 0 };
    };
    //=============================================================================
    CvodesSession::CvodesSession() : impl_(new Impl()) { }
    //=============================================================================
    CvodesSession::~CvodesSession() { delete impl_; }
    //=============================================================================
    bool
    CvodesSession::init(RhsFn rhs, RootFn roots, const std::vector<double>& y0, double t0,
        const CvodesOptions& opts, std::string& err)
    {
        Impl& p = *impl_;
        p.n = static_cast<int>(y0.size());
        if (p.n == 0) {
            err = "empty state vector";
            return false;
        }
        p.rhs = std::move(rhs);
        p.roots = std::move(roots);
        p.cb.rhs = &p.rhs;
        p.cb.roots = &p.roots;
        p.numRoots = opts.numRoots;
        return setup(p.res, p.cb, p.n, y0, t0, opts, err);
    }
    //=============================================================================
    CvodesSession::AdvanceOut
    CvodesSession::advanceTo(double tStop, double* x)
    {
        Impl& p = *impl_;
        AdvanceOut out;
        sunrealtype tret = 0.0;
        const int flag = CVode(p.res.mem, tStop, p.res.y, &tret, CV_NORMAL);
        out.t = static_cast<double>(tret);
        if (flag == CV_ROOT_RETURN) {
            std::vector<int> found(p.numRoots, 0);
            CVodeGetRootInfo(p.res.mem, found.data());
            for (int i = 0; i < p.numRoots; ++i) {
                if (found[i] != 0) {
                    out.rootsFound.push_back(i);
                }
            }
            out.status = CvodesStatus::RootFound;
        } else if (flag < 0) {
            out.status = mapFlag(flag);
            out.message = "CVode failed with flag " + std::to_string(flag);
        } else {
            out.status = CvodesStatus::Success;
        }
        const double* yd = NV_DATA_S(p.res.y);
        std::copy(yd, yd + p.n, x);
        return out;
    }
    //=============================================================================
    bool
    CvodesSession::reinit(double t, const double* x, std::string& err)
    {
        Impl& p = *impl_;
        std::copy(x, x + p.n, NV_DATA_S(p.res.y));
        if (CVodeReInit(p.res.mem, t, p.res.y) != CV_SUCCESS) {
            err = "CVodeReInit failed";
            return false;
        }
        return true;
    }
    //=============================================================================
    CvodesStats
    CvodesSession::stats() const
    {
        CvodesStats s;
        if (impl_ != nullptr && impl_->res.mem != nullptr) {
            collectStats(impl_->res.mem, s);
        }
        return s;
    }
    //=============================================================================
    // IDAS (differential-algebraic) session: the residual-form analogue of
    // CvodesSession, with the same pimpl structure. SUNDIALS types stay in this
    // translation unit.
    namespace {
        struct IdasCallbackContext
        {
            const ResidualFn* residual = nullptr;
            const RootFn* roots = nullptr;
        };
        //=============================================================================
        int
        idasResidualTrampoline(sunrealtype t, N_Vector y, N_Vector yp, N_Vector rr, void* user)
        {
            auto* c = static_cast<IdasCallbackContext*>(user);
            return (*c->residual)(
                       static_cast<double>(t), NV_DATA_S(y), NV_DATA_S(yp), NV_DATA_S(rr))
                ? 0
                : 1;
        }
        //=============================================================================
        int
        idasRootTrampoline(sunrealtype t, N_Vector y, N_Vector, sunrealtype* gout, void* user)
        {
            auto* c = static_cast<IdasCallbackContext*>(user);
            return (*c->roots)(static_cast<double>(t), NV_DATA_S(y), gout) ? 0 : 1;
        }
        //=============================================================================
        struct IdasResources
        {
            SUNContext ctx { nullptr };
            N_Vector y { nullptr };
            N_Vector yp { nullptr };
            N_Vector absTol { nullptr };
            N_Vector id { nullptr };
            SUNMatrix A { nullptr };
            SUNLinearSolver LS { nullptr };
            void* mem { nullptr };

            ~IdasResources()
            {
                if (mem != nullptr) {
                    IDAFree(&mem);
                }
                if (LS != nullptr) {
                    SUNLinSolFree(LS);
                }
                if (A != nullptr) {
                    SUNMatDestroy(A);
                }
                if (id != nullptr) {
                    N_VDestroy_Serial(id);
                }
                if (absTol != nullptr) {
                    N_VDestroy_Serial(absTol);
                }
                if (yp != nullptr) {
                    N_VDestroy_Serial(yp);
                }
                if (y != nullptr) {
                    N_VDestroy_Serial(y);
                }
                if (ctx != nullptr) {
                    SUNContext_Free(&ctx);
                }
            }
        };
        //=============================================================================
        CvodesStatus
        mapIdaFlag(int flag)
        {
            switch (flag) {
            case IDA_TOO_MUCH_WORK:
                return CvodesStatus::TooMuchWork;
            case IDA_TOO_MUCH_ACC:
            case IDA_ERR_FAIL:
                return CvodesStatus::TolTooSmall;
            case IDA_RES_FAIL:
            case IDA_FIRST_RES_FAIL:
            case IDA_REP_RES_ERR:
                return CvodesStatus::RhsFailure;
            case IDA_LSETUP_FAIL:
            case IDA_LSOLVE_FAIL:
                return CvodesStatus::LinearSolverFailure;
            default:
                return CvodesStatus::Error;
            }
        }
        //=============================================================================
        bool
        idasSetup(IdasResources& res, IdasCallbackContext& cb, int n, const std::vector<double>& y0,
            const std::vector<double>& yp0, double t0, double tFirst, const IdasOptions& opts,
            std::string& err)
        {
            if (SUNContext_Create(SUN_COMM_NULL, &res.ctx) != 0) {
                err = "SUNContext_Create failed";
                return false;
            }
            res.y = N_VNew_Serial(n, res.ctx);
            res.yp = N_VNew_Serial(n, res.ctx);
            if (res.y == nullptr || res.yp == nullptr) {
                err = "N_VNew_Serial failed";
                return false;
            }
            std::copy(y0.begin(), y0.end(), NV_DATA_S(res.y));
            if (static_cast<int>(yp0.size()) == n) {
                std::copy(yp0.begin(), yp0.end(), NV_DATA_S(res.yp));
            } else {
                std::fill_n(NV_DATA_S(res.yp), n, 0.0);
            }

            res.mem = IDACreate(res.ctx);
            if (res.mem == nullptr) {
                err = "IDACreate failed";
                return false;
            }
            if (IDAInit(res.mem, idasResidualTrampoline, t0, res.y, res.yp) != IDA_SUCCESS) {
                err = "IDAInit failed";
                return false;
            }
            if (IDASetUserData(res.mem, &cb) != IDA_SUCCESS) {
                err = "IDASetUserData failed";
                return false;
            }

            const double rel = opts.relTol > 0.0 ? opts.relTol : 1e-6;
            if (static_cast<int>(opts.absTol.size()) == n) {
                res.absTol = N_VNew_Serial(n, res.ctx);
                std::copy(opts.absTol.begin(), opts.absTol.end(), NV_DATA_S(res.absTol));
                if (IDASVtolerances(res.mem, rel, res.absTol) != IDA_SUCCESS) {
                    err = "IDASVtolerances failed";
                    return false;
                }
            } else {
                const double atol
                    = (opts.absTol.size() == 1 && opts.absTol[0] > 0.0) ? opts.absTol[0] : 1e-8;
                if (IDASStolerances(res.mem, rel, atol) != IDA_SUCCESS) {
                    err = "IDASStolerances failed";
                    return false;
                }
            }

            res.A = SUNDenseMatrix(n, n, res.ctx);
            res.LS = SUNLinSol_Dense(res.y, res.A, res.ctx);
            if (res.A == nullptr || res.LS == nullptr
                || IDASetLinearSolver(res.mem, res.LS, res.A) != IDA_SUCCESS) {
                err = "IDAS dense linear solver setup failed";
                return false;
            }

            if (opts.maxNumSteps > 0) {
                IDASetMaxNumSteps(res.mem, opts.maxNumSteps);
            }
            if (opts.initStep > 0.0) {
                IDASetInitStep(res.mem, opts.initStep);
            }
            if (opts.maxStep > 0.0) {
                IDASetMaxStep(res.mem, opts.maxStep);
            }

            // Consistent initial conditions: with an id vector (1 = differential,
            // 0 = algebraic) IDACalcIC(IDA_YA_YDP_INIT) solves for the algebraic y
            // components and the differential y' at t0, advancing toward tFirst.
            if (static_cast<int>(opts.id.size()) == n) {
                res.id = N_VNew_Serial(n, res.ctx);
                std::copy(opts.id.begin(), opts.id.end(), NV_DATA_S(res.id));
                if (IDASetId(res.mem, res.id) != IDA_SUCCESS) {
                    err = "IDASetId failed";
                    return false;
                }
                if (opts.calcIC) {
                    const double tout1 = (tFirst != t0) ? tFirst : (t0 + 1.0);
                    if (IDACalcIC(res.mem, IDA_YA_YDP_INIT, tout1) != IDA_SUCCESS) {
                        err = "IDACalcIC failed (inconsistent initial conditions)";
                        return false;
                    }
                    IDAGetConsistentIC(res.mem, res.y, res.yp);
                }
            }

            if (opts.numRoots > 0) {
                if (IDARootInit(res.mem, opts.numRoots, idasRootTrampoline) != IDA_SUCCESS) {
                    err = "IDARootInit failed";
                    return false;
                }
                if (static_cast<int>(opts.rootDirection.size()) == opts.numRoots) {
                    IDASetRootDirection(res.mem, const_cast<int*>(opts.rootDirection.data()));
                }
            }
            return true;
        }
        //=============================================================================
    } // namespace
    //=============================================================================
    struct IdasSession::Impl
    {
        IdasResources res;
        ResidualFn residual;
        RootFn roots;
        IdasCallbackContext cb;
        int n { 0 };
        int numRoots { 0 };
        bool calcIC { false }; // recompute consistent ICs on reinit (DAE path only)
        double maxStep { 0.0 }; // captured max step, bounds the calcIC forward instant
    };
    //=============================================================================
    IdasSession::IdasSession() : impl_(new Impl()) { }
    //=============================================================================
    IdasSession::~IdasSession() { delete impl_; }
    //=============================================================================
    bool
    IdasSession::init(ResidualFn residual, RootFn roots, const std::vector<double>& y0,
        const std::vector<double>& yp0, double t0, double tFirst, const IdasOptions& opts,
        std::string& err)
    {
        Impl& p = *impl_;
        p.n = static_cast<int>(y0.size());
        if (p.n == 0) {
            err = "empty state vector";
            return false;
        }
        p.residual = std::move(residual);
        p.roots = std::move(roots);
        p.cb.residual = &p.residual;
        p.cb.roots = &p.roots;
        p.numRoots = opts.numRoots;
        p.calcIC = opts.calcIC;
        p.maxStep = opts.maxStep;
        return idasSetup(p.res, p.cb, p.n, y0, yp0, t0, tFirst, opts, err);
    }
    //=============================================================================
    IdasSession::AdvanceOut
    IdasSession::advanceTo(double tStop, double* y, double* yp)
    {
        Impl& p = *impl_;
        AdvanceOut out;
        sunrealtype tret = 0.0;
        const int flag = IDASolve(p.res.mem, tStop, &tret, p.res.y, p.res.yp, IDA_NORMAL);
        out.t = static_cast<double>(tret);
        if (flag == IDA_ROOT_RETURN) {
            std::vector<int> found(p.numRoots, 0);
            IDAGetRootInfo(p.res.mem, found.data());
            for (int i = 0; i < p.numRoots; ++i) {
                if (found[i] != 0) {
                    out.rootsFound.push_back(i);
                }
            }
            out.status = CvodesStatus::RootFound;
        } else if (flag < 0) {
            out.status = mapIdaFlag(flag);
            out.message = "IDASolve failed with flag " + std::to_string(flag);
        } else {
            out.status = CvodesStatus::Success;
        }
        const double* yd = NV_DATA_S(p.res.y);
        std::copy(yd, yd + p.n, y);
        if (yp != nullptr) {
            const double* ypd = NV_DATA_S(p.res.yp);
            std::copy(ypd, ypd + p.n, yp);
        }
        return out;
    }
    //=============================================================================
    bool
    IdasSession::getState(double* y, double* yp) const
    {
        const Impl& p = *impl_;
        if (p.n == 0 || p.res.y == nullptr) {
            return false;
        }
        const double* yd = NV_DATA_S(p.res.y);
        std::copy(yd, yd + p.n, y);
        if (yp != nullptr && p.res.yp != nullptr) {
            const double* ypd = NV_DATA_S(p.res.yp);
            std::copy(ypd, ypd + p.n, yp);
        }
        return true;
    }
    //=============================================================================
    bool
    IdasSession::reinit(double t, const double* y, const double* yp, std::string& err)
    {
        Impl& p = *impl_;
        std::copy(y, y + p.n, NV_DATA_S(p.res.y));
        if (yp != nullptr) {
            std::copy(yp, yp + p.n, NV_DATA_S(p.res.yp));
        }
        if (IDAReInit(p.res.mem, t, p.res.y, p.res.yp) != IDA_SUCCESS) {
            err = "IDAReInit failed";
            return false;
        }
        // A cold restart discards the multistep history but keeps the algebraic
        // components at their pre-restart values. After a discontinuity (e.g. a
        // step source driving an acausal island) they no longer satisfy F = 0 and
        // IDA's first post-restart step fails. With an id vector installed at
        // setup (it survives IDAReInit), re-solve the algebraic y and all y' for
        // the new configuration, as idasSetup() does at t0.
        if (p.calcIC && p.res.id != nullptr) {
            const double h = (p.maxStep > 0.0) ? std::min(p.maxStep, 1.0e-3 * (1.0 + std::fabs(t)))
                                               : 1.0e-3 * (1.0 + std::fabs(t));
            const double tout1 = t + h;
            if (IDACalcIC(p.res.mem, IDA_YA_YDP_INIT, tout1) != IDA_SUCCESS) {
                err = "IDACalcIC failed (inconsistent initial conditions after "
                      "discontinuity at t="
                    + std::to_string(t) + ")";
                return false;
            }
            IDAGetConsistentIC(p.res.mem, p.res.y, p.res.yp);
        }
        return true;
    }
    //=============================================================================
    void
    IdasSession::setStopTime(double t)
    {
        Impl& p = *impl_;
        if (p.res.mem != nullptr) {
            IDASetStopTime(p.res.mem, t);
        }
    }
    //=============================================================================
    CvodesStats
    IdasSession::stats() const
    {
        CvodesStats s;
        if (impl_ != nullptr && impl_->res.mem != nullptr) {
            long v = 0;
            if (IDAGetNumSteps(impl_->res.mem, &v) == IDA_SUCCESS) {
                s.steps = v;
            }
            if (IDAGetNumResEvals(impl_->res.mem, &v) == IDA_SUCCESS) {
                s.rhsEvals = v;
            }
            if (IDAGetNumErrTestFails(impl_->res.mem, &v) == IDA_SUCCESS) {
                s.errTestFails = v;
            }
            sunrealtype h = 0.0;
            if (IDAGetLastStep(impl_->res.mem, &h) == IDA_SUCCESS) {
                s.lastStep = static_cast<double>(h);
            }
        }
        return s;
    }
    //=============================================================================
} // namespace OdeCore
//=============================================================================
} // namespace Nelson
//=============================================================================
#endif
//=============================================================================
