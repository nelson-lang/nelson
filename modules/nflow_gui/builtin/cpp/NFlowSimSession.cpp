//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowSimSession.hpp"
#include "NFlowSimulationRun.hpp"
#include "BackgroundWorkerPool.hpp"
#include "NFlowNelsonBridge.hpp"
#include "SimEventBridge.hpp"
#include <nlohmann/json.hpp>
#include <atomic>
#include <vector>
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// Poll deltas accumulate while the UI stalls; bound the queue.
static constexpr std::size_t MAX_PENDING_PARTIALS = 64;
//=============================================================================
static const char*
deliveryLabel(NFlowSimDelivery mode)
{
    switch (mode) {
    case NFlowSimDelivery::Poll:
        return "poll";
    case NFlowSimDelivery::Both:
        return "both";
    case NFlowSimDelivery::Push:
    default:
        return "push";
    }
}
//=============================================================================
NFlowSimSession::NFlowSimSession(Evaluator* eval,
    std::function<void(const std::string&, const std::string&)> push, BackgroundWorkerPool* workers,
    std::function<NFlowSimDelivery()> deliveryMode, std::function<void()> prepareInterpreter)
    : m_eval(eval)
    , m_push(std::move(push))
    , m_workers(workers)
    , m_deliveryMode(std::move(deliveryMode))
    , m_prepareInterpreter(std::move(prepareInterpreter))
{
}
//=============================================================================
void
NFlowSimSession::setDiagnosticHooks(std::function<void(const std::string&)> onAlreadyRunning,
    std::function<void(const std::string&)> onSetRunning, std::function<void()> onClearRunning)
{
    m_onAlreadyRunning = std::move(onAlreadyRunning);
    m_onSetRunning = std::move(onSetRunning);
    m_onClearRunning = std::move(onClearRunning);
}
//=============================================================================
void
NFlowSimSession::setOwnerAliveCheck(std::function<bool()> ownerAlive)
{
    m_ownerAlive = std::move(ownerAlive);
}
//=============================================================================
std::string
NFlowSimSession::runBody(const std::string& diagramJson, NFlowSimDelivery mode, bool prepareState)
{
    // Install interpreter-thread state (e.g. the workspace bridge) once, before the
    // first numeric run on any path, so the core finds it already in place. The
    // window leaves this empty (the bridge is installed at window creation); the
    // headless host marshals it to the interpreter thread here.
    if (m_prepareInterpreter) {
        std::call_once(m_prepareOnce, [this]() { m_prepareInterpreter(); });
    }

    // Always detach the event callback and clear the running state, even on failure.
    struct RunTeardown
    {
        NFlowSimSession* self;
        ~RunTeardown()
        {
            {
                std::lock_guard<std::mutex> lock(self->m_eventsMutex);
                self->m_eventCallback = nullptr;
            }
            Nelson::NFlow::setSimEventFetcher(
                std::function<std::vector<Nelson::NFlow::SimEvent>()>());
            self->m_running.store(false);
            self->m_cancel.store(false);
            if (self->m_onClearRunning) {
                self->m_onClearRunning();
            }
        }
    } runTeardown { this };

    if (prepareState) {
        m_cancel.store(false);
        m_paused.store(false);
        m_running.store(true);
    }

    // Shared queue keeps the event callback independent from this stack.
    struct SimEventQueue
    {
        std::deque<std::pair<std::string, std::string>> q;
        std::mutex m;
    };
    auto simEv = std::make_shared<SimEventQueue>();

    {
        std::lock_guard<std::mutex> lock(m_eventsMutex);
        m_eventCallback = [simEv](const std::string& name, const std::string& data) {
            std::lock_guard<std::mutex> lock(simEv->m);
            simEv->q.emplace_back(name, data);
        };
    }

    Nelson::NFlow::setSimEventFetcher([simEv]() -> std::vector<Nelson::NFlow::SimEvent> {
        std::vector<Nelson::NFlow::SimEvent> out;
        std::lock_guard<std::mutex> lock(simEv->m);
        while (!simEv->q.empty()) {
            out.emplace_back(simEv->q.front().first, simEv->q.front().second);
            simEv->q.pop_front();
        }
        return out;
    });

    const bool accumulate = (mode == NFlowSimDelivery::Poll || mode == NFlowSimDelivery::Both);
    const bool emitPush = (mode == NFlowSimDelivery::Push || mode == NFlowSimDelivery::Both);

    NFlowSimRunHooks hooks;
    hooks.onProgress = [this, accumulate, emitPush](const std::string& partialJson) {
        if (accumulate) {
            std::lock_guard<std::mutex> lock(m_resultMutex);
            if (m_partials.size() >= MAX_PENDING_PARTIALS) {
                m_partials.pop_front();
            }
            m_partials.push_back(partialJson);
        }
        if (emitPush && m_push) {
            m_push("nflow.sim.partial", partialJson);
        }
    };
    hooks.isCancelled = [this]() -> bool { return m_cancel.load(); };
    hooks.isPaused = [this]() -> bool { return m_paused.load(); };
    hooks.runStopFcnInline = prepareState;

    return runNFlowSimulationCore(m_eval, diagramJson, hooks);
}
//=============================================================================
std::string
NFlowSimSession::runSync(const std::string& diagramJson)
{
    // Synchronous run streams partials by push, like the editor's inline path.
    return runBody(diagramJson, NFlowSimDelivery::Push, true);
}
//=============================================================================
std::string
NFlowSimSession::startAsync(const std::string& diagramJson)
{
    static std::atomic<unsigned long long> nextRunId { 1 };
    const std::string runId = "sim-" + std::to_string(nextRunId.fetch_add(1));

    bool expected = false;
    if (!m_running.compare_exchange_strong(expected, true)) {
        if (m_onAlreadyRunning) {
            m_onAlreadyRunning(runId);
        }
        return nlohmann::json { { "status", "error" }, { "reason", "already_running" } }.dump();
    }
    if (m_onSetRunning) {
        m_onSetRunning(runId);
    }
    m_cancel.store(false);
    m_paused.store(false);

    const NFlowSimDelivery mode = m_deliveryMode ? m_deliveryMode() : NFlowSimDelivery::Push;
    const bool accumulate = (mode == NFlowSimDelivery::Poll || mode == NFlowSimDelivery::Both);
    const bool emitPush = (mode == NFlowSimDelivery::Push || mode == NFlowSimDelivery::Both);

    if (accumulate) {
        std::lock_guard<std::mutex> lock(m_resultMutex);
        m_resultRunId = runId;
        m_resultJson.clear();
        m_partials.clear();
        m_resultReady = false;
    }

    std::weak_ptr<NFlowSimSession> weakSelf = weak_from_this();
    bool launched = false;
    try {
        launched = m_workers->start([weakSelf, diagramJson, runId, mode, accumulate, emitPush]() {
            auto self = weakSelf.lock();
            if (!self) {
                return;
            }
            const std::string result = self->runBody(diagramJson, mode, false);

            // The owner (a window) may have gone while the run finished: skip
            // delivery and StopFcn, exactly like the editor's worker guard.
            if (self->m_ownerAlive && !self->m_ownerAlive()) {
                return;
            }

            if (accumulate) {
                std::lock_guard<std::mutex> lock(self->m_resultMutex);
                if (self->m_resultRunId == runId) {
                    self->m_resultJson = result;
                    self->m_resultReady = true;
                }
            }
            if (emitPush && self->m_push) {
                nlohmann::json payload;
                payload["runId"] = runId;
                try {
                    payload["result"] = nlohmann::json::parse(result);
                } catch (...) {
                    payload["resultJson"] = result;
                }
                self->m_push("nflow.sim.final", payload.dump());
            }

            // Run the model StopFcn after delivering the final result, so a figure
            // cannot delay the last frame.
            std::string stopFcnError;
            nflowRunStopFcnViaInterpreter(diagramJson, result, stopFcnError);
        });
    } catch (...) {
        m_running.store(false);
        throw;
    }
    if (!launched) {
        m_running.store(false);
        return nlohmann::json { { "status", "error" }, { "reason", "shutting_down" } }.dump();
    }

    nlohmann::json started;
    started["status"] = "started";
    started["runId"] = runId;
    started["delivery"] = deliveryLabel(mode);
    return started.dump();
}
//=============================================================================
std::string
NFlowSimSession::poll(const std::string& runId)
{
    std::lock_guard<std::mutex> lock(m_resultMutex);
    const bool matchesRun = m_resultRunId == runId;
    const bool complete = matchesRun && m_resultReady;
    // Hand-assembled response: queued partials and the final result are
    // engine-produced JSON already, so splice them in verbatim.
    std::string response = "{\"status\":\"";
    response += complete ? "complete" : "pending";
    response += "\",\"runId\":";
    response += nlohmann::json(runId).dump();
    response += ",\"partials\":[";
    bool firstPartial = true;
    while (matchesRun && !m_partials.empty()) {
        const std::string partialJson = std::move(m_partials.front());
        m_partials.pop_front();
        if (!partialJson.empty() && (partialJson[0] == '{' || partialJson[0] == '[')) {
            if (!firstPartial) {
                response += ",";
            }
            firstPartial = false;
            response += partialJson;
        }
    }
    response += "]";
    if (complete) {
        if (!m_resultJson.empty() && (m_resultJson[0] == '{' || m_resultJson[0] == '[')) {
            response += ",\"result\":";
            response += m_resultJson;
        } else {
            response += ",\"resultJson\":";
            response += nlohmann::json(m_resultJson).dump();
        }
    }
    response += "}";
    return response;
}
//=============================================================================
void
NFlowSimSession::pause(bool wantPause)
{
    m_paused.store(wantPause);
}
//=============================================================================
bool
NFlowSimSession::stop()
{
    if (!m_running.load()) {
        return false;
    }
    m_cancel.store(true);
    return true;
}
//=============================================================================
bool
NFlowSimSession::isRunning() const
{
    return m_running.load();
}
//=============================================================================
void
NFlowSimSession::sendEvent(const std::string& name, const std::string& data)
{
    std::lock_guard<std::mutex> lock(m_eventsMutex);
    if (m_eventCallback) {
        m_eventCallback(name, data);
    } else {
        m_events.emplace_back(name, data);
    }
}
//=============================================================================
void
NFlowSimSession::shutdown(const std::function<void()>& drainPoll)
{
    m_cancel.store(true);
    if (m_workers != nullptr) {
        m_workers->stopAndJoin(drainPoll);
    }
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
