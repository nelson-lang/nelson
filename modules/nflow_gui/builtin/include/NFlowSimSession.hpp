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
#include <atomic>
#include <deque>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <utility>
//=============================================================================
namespace Nelson {
class Evaluator;
//=============================================================================
} // namespace Nelson
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
class BackgroundWorkerPool;
//=============================================================================
// How a run's partial deltas and final result reach the client.
enum class NFlowSimDelivery
{
    // Stream nflow.sim.partial / nflow.sim.final through the push sink.
    Push,
    // Accumulate partials and the final result for nflow.sim.poll.
    Poll,
    // Both of the above (headless kernel: lets the client pick notifications or poll).
    Both
};
//=============================================================================
// Run-state orchestration for one NFlow simulation surface, shared by the editor
// window and the headless kernel host. It owns the runId, the partial
// accumulation, the pause and cancel flags and the detached worker, and drives
// the numeric run through runNFlowSimulationCore. A window binds its push sink to
// window.__nelsonPush and a poll/push decider to its live window state; the
// headless host binds the push sink to its C callback and asks for Both.
// Managed by shared_ptr: a detached worker keeps the session alive through its
// result delivery by locking weak_from_this, so the owner (a pruned window) may
// release its reference while a run finishes. Internal to nflow_gui_builtin (the
// window and the kernel host both live here); only the extern "C" host is public.
class NFlowSimSession : public std::enable_shared_from_this<NFlowSimSession>
{
public:
    // eval: evaluator whose base workspace the simulation reads and writes.
    // push: emits a channel plus a JSON payload to the client; may be invoked from
    //   a worker thread.
    // workers: pool the detached async run launches on, owned by the caller and
    //   outliving the session.
    // deliveryMode: evaluated at each async start to pick the delivery for that run
    //   (a window reads its live state here; the headless host returns Both).
    // prepareInterpreter: run once before the first async worker starts, so a host
    //   can install state that must live on the interpreter thread (the headless
    //   host marshals ensureNelsonWorkspaceBridge here); empty for the window, which
    //   installs the bridge at window creation.
    NFlowSimSession(Evaluator* eval,
        std::function<void(const std::string& channel, const std::string& payloadJson)> push,
        BackgroundWorkerPool* workers, std::function<NFlowSimDelivery()> deliveryMode,
        std::function<void()> prepareInterpreter = {});

    // Optional diagnostic hooks, fired at the run-state transitions the editor logs
    // (the headless host leaves them empty).
    void
    setDiagnosticHooks(std::function<void(const std::string& runId)> onAlreadyRunning,
        std::function<void(const std::string& runId)> onSetRunning,
        std::function<void()> onClearRunning);

    // Optional owner-liveness check: when it returns false after a run completes,
    // the worker skips result delivery and StopFcn (the editor sets it so a closed
    // window neither pushes nor runs StopFcn). Empty means always deliver.
    void
    setOwnerAliveCheck(std::function<bool()> ownerAlive);

    // nflow.sim.run async: start the detached worker; returns a started/error JSON.
    std::string
    startAsync(const std::string& diagramJson);
    // nflow.sim.run sync: run inline and return the result JSON.
    std::string
    runSync(const std::string& diagramJson);
    // nflow.sim.poll: status plus any queued partials and the final result.
    std::string
    poll(const std::string& runId);
    void
    pause(bool wantPause);
    // nflow.sim.stop: request cancellation of a running run.
    bool
    stop();
    bool
    isRunning() const;
    void
    sendEvent(const std::string& name, const std::string& data);
    // Signal cancel and join the worker. drainPoll, when set, runs while joining so
    // a worker blocked on an interpreter hand-off can complete (the editor drains
    // MainThreadTaskQueue here).
    void
    shutdown(const std::function<void()>& drainPoll = {});

private:
    // The numeric run plus its event wiring. pushProgress-style delivery is chosen
    // by mode; prepareState runs StopFcn inline and manages the running flag for a
    // synchronous run.
    std::string
    runBody(const std::string& diagramJson, NFlowSimDelivery mode, bool prepareState);

    Evaluator* m_eval = nullptr;
    std::function<void(const std::string&, const std::string&)> m_push;
    BackgroundWorkerPool* m_workers = nullptr;
    std::function<NFlowSimDelivery()> m_deliveryMode;
    std::function<void()> m_prepareInterpreter;
    std::once_flag m_prepareOnce;

    std::function<void(const std::string&)> m_onAlreadyRunning;
    std::function<void(const std::string&)> m_onSetRunning;
    std::function<void()> m_onClearRunning;
    std::function<bool()> m_ownerAlive;

    std::atomic<bool> m_cancel { false };
    std::atomic<bool> m_running { false };
    std::atomic<bool> m_paused { false };

    std::mutex m_resultMutex;
    std::string m_resultRunId;
    std::string m_resultJson;
    std::deque<std::string> m_partials;
    bool m_resultReady = false;

    std::mutex m_eventsMutex;
    std::deque<std::pair<std::string, std::string>> m_events;
    std::function<void(const std::string& name, const std::string& data)> m_eventCallback;
};
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
