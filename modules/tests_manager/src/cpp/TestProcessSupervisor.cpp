//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if defined(_MSC_VER)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#endif
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <condition_variable>
#include <cstdlib>
#include <deque>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <memory>
#include <mutex>
#include <numeric>
#include <optional>
#include <string>
#include <thread>
#include <vector>
#include "characters_encoding.hpp"
#include "TestChildProcess.hpp"
#include "TestProcessSupervisor.hpp"
//=============================================================================
namespace Nelson::TestsManager {
//=============================================================================
namespace {
    //=============================================================================
    using Clock = std::chrono::steady_clock;
    constexpr const char* RESULT_SENTINEL = "__NELSON_UNITTEST_RESULT__";
    //=============================================================================
    struct SupervisorSession
    {
        std::vector<TestProcessJob> jobs;
        std::deque<size_t> directQueue;
        std::deque<size_t> reusableQueue;
        std::vector<std::thread> threads;
        std::deque<TestProcessEvent> events;
        std::mutex queueMutex;
        std::mutex eventMutex;
        std::mutex joinMutex;
        std::condition_variable eventCondition;
        std::atomic_size_t completed { 0 };
        std::atomic_uint64_t nextSequence { 1 };
        std::atomic_bool interrupted { false };
        Clock::time_point started = Clock::now();
        uint64 handle = 0;
        bool joined = false;
        bool doneEmitted = false;
    };
    //=============================================================================
    std::mutex supervisorMutex;
    std::mutex traceMutex;
    std::map<uint64, std::shared_ptr<SupervisorSession>> supervisors;
    uint64 nextSupervisorHandle = 1;
    //=============================================================================
    bool
    traceFlagSet(const char* name)
    {
#if defined(_MSC_VER)
        // setenv() updates the Win32 process block only; the CRT copy read by
        // std::getenv/_dupenv_s is never refreshed, so query Win32 directly.
        char value[8] = { 0 };
        DWORD length = ::GetEnvironmentVariableA(name, value, sizeof(value));
        return length == 1 && value[0] == '1';
#else
        const char* value = std::getenv(name);
        return value != nullptr && std::string(value) == "1";
#endif
    }
    //=============================================================================
    bool
    traceEnabled()
    {
        return traceFlagSet("NELSON_UNITTEST_TRACE")
            || traceFlagSet("NELSON_UNITTEST_TRACE_PROGRESSIVE");
    }
    //=============================================================================
    void
    trace(const std::shared_ptr<SupervisorSession>& session, const std::string& message)
    {
        if (!traceEnabled()) {
            return;
        }
        auto elapsed
            = std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - session->started)
                  .count();
        std::lock_guard<std::mutex> lock(traceMutex);
        std::cerr << "[tests_manager +" << elapsed << "ms session=" << session->handle << "] "
                  << message << std::endl;
    }
    //=============================================================================
    std::string
    jobLabel(const TestProcessJob& job)
    {
        return "job=" + wstring_to_utf8(job.jobId) + " case=" + std::to_string(job.index)
            + " kind=" + wstring_to_utf8(job.kind);
    }
    //=============================================================================
    uint64
    elapsedMilliseconds(Clock::time_point started)
    {
        return static_cast<uint64>(
            std::chrono::duration_cast<std::chrono::milliseconds>(Clock::now() - started).count());
    }
    //=============================================================================
    void
    pushEvent(const std::shared_ptr<SupervisorSession>& session, TestProcessEvent event)
    {
        event.sequence = session->nextSequence.fetch_add(1);
        {
            std::lock_guard<std::mutex> lock(session->eventMutex);
            session->events.push_back(std::move(event));
        }
        session->eventCondition.notify_all();
    }
    //=============================================================================
    void
    caseCompleted(const std::shared_ptr<SupervisorSession>& session)
    {
        session->completed.fetch_add(1);
        session->eventCondition.notify_all();
    }
    //=============================================================================
    std::vector<std::string>
    takePayloads(std::string& pending)
    {
        std::vector<std::string> payloads;
        const std::string sentinel(RESULT_SENTINEL);
        while (true) {
            size_t start = pending.find(sentinel);
            if (start == std::string::npos) {
                if (pending.size() > sentinel.size()) {
                    pending.erase(0, pending.size() - sentinel.size());
                }
                break;
            }
            size_t payloadStart = start + sentinel.size();
            size_t end = pending.find_first_of("\r\n", payloadStart);
            if (end == std::string::npos) {
                if (start > 0) {
                    pending.erase(0, start);
                }
                break;
            }
            payloads.push_back(pending.substr(payloadStart, end - payloadStart));
            size_t next = end + 1;
            if (pending[end] == '\r' && next < pending.size() && pending[next] == '\n') {
                ++next;
            }
            pending.erase(0, next);
        }
        return payloads;
    }
    //=============================================================================
    std::optional<std::string>
    readCompletedResultFile(const std::wstring& filename)
    {
        if (filename.empty()) {
            return std::nullopt;
        }
        std::ifstream input(std::filesystem::path(filename), std::ios::binary);
        if (!input) {
            return std::nullopt;
        }
        std::string payload(
            (std::istreambuf_iterator<char>(input)), std::istreambuf_iterator<char>());
        if (payload.empty() || payload.back() != '\n') {
            return std::nullopt;
        }
        payload.pop_back();
        if (!payload.empty() && payload.back() == '\r') {
            payload.pop_back();
        }
        return payload;
    }
    //=============================================================================
    bool
    jobPriority(const std::vector<TestProcessJob>& jobs, size_t left, size_t right)
    {
        if (jobs[left].weight != jobs[right].weight) {
            return jobs[left].weight > jobs[right].weight;
        }
        if (jobs[left].scheduleOrder != jobs[right].scheduleOrder) {
            return jobs[left].scheduleOrder < jobs[right].scheduleOrder;
        }
        return jobs[left].inputPosition < jobs[right].inputPosition;
    }
    //=============================================================================
    std::optional<size_t>
    popJob(const std::shared_ptr<SupervisorSession>& session, bool reusable)
    {
        std::lock_guard<std::mutex> lock(session->queueMutex);
        std::deque<size_t>& queue = reusable ? session->reusableQueue : session->directQueue;
        if (queue.empty()) {
            return std::nullopt;
        }
        size_t value = queue.front();
        queue.pop_front();
        return value;
    }
    //=============================================================================
    TestProcessResult
    baseResult(const TestChildProcess& process)
    {
        TestProcessResult result;
        result.pid = process.pid();
        result.processPolicy = process.policy();
        return result;
    }
    //=============================================================================
    void
    emitPayload(const std::shared_ptr<SupervisorSession>& session, const TestProcessJob& job,
        const TestProcessResult& result, const std::string& payload, uint64 workerId)
    {
        TestProcessEvent event;
        event.eventType = L"payload";
        event.job = job;
        event.result = result;
        event.payload = utf8_to_wstring(payload);
        event.workerId = workerId;
        trace(session, "payload worker=" + std::to_string(workerId) + " " + jobLabel(job));
        pushEvent(session, std::move(event));
    }
    //=============================================================================
    void
    emitProcess(const std::shared_ptr<SupervisorSession>& session, const TestProcessJob& job,
        const TestProcessResult& result, uint64 workerId)
    {
        TestProcessEvent event;
        event.eventType = L"process";
        event.job = job;
        event.result = result;
        event.workerId = workerId;
        trace(session,
            "process worker=" + std::to_string(workerId)
                + " reason=" + wstring_to_utf8(result.reason) + " " + jobLabel(job));
        pushEvent(session, std::move(event));
    }
    //=============================================================================
    bool
    hasTimedOut(const TestProcessJob& job, Clock::time_point started)
    {
        return job.timeout != 0
            && std::chrono::duration_cast<std::chrono::seconds>(Clock::now() - started).count()
            >= static_cast<int64_t>(job.timeout);
    }
    //=============================================================================
    void
    readProcessOutput(TestChildProcess& process, std::string& output, std::string& error,
        std::string& pending, std::vector<std::string>& payloads)
    {
        std::string chunk;
        process.readAvailable(chunk, error);
        if (!chunk.empty()) {
            output.append(chunk);
            pending.append(chunk);
            std::vector<std::string> parsed = takePayloads(pending);
            payloads.insert(payloads.end(), parsed.begin(), parsed.end());
        }
    }
    //=============================================================================
    TestProcessResult
    runDirectProcess(const std::shared_ptr<SupervisorSession>& session, const TestProcessJob& job,
        uint64 workerId)
    {
        TestProcessResult result;
        TestChildProcess process;
        std::wstring launchError;
        Clock::time_point started = Clock::now();
        if (!process.start(job.executable, job.arguments, false, launchError)) {
            result.error = launchError;
            result.reason = L"launch_error";
            result.duration = elapsedMilliseconds(started);
            trace(session, "launch_error " + wstring_to_utf8(launchError) + " " + jobLabel(job));
            return result;
        }
        result = baseResult(process);
        trace(session,
            "start worker=" + std::to_string(workerId) + " pid=" + std::to_string(result.pid) + " "
                + jobLabel(job));
        std::string output;
        std::string error;
        std::string pending;
        int exitCode = -1;
        bool exited = false;
        bool completedFromFile = false;
        while (!session->interrupted.load()) {
            std::vector<std::string> payloads;
            readProcessOutput(process, output, error, pending, payloads);
            TestProcessResult partial = result;
            partial.duration = elapsedMilliseconds(started);
            partial.reason = L"payload";
            for (const std::string& payload : payloads) {
                emitPayload(session, job, partial, payload, workerId);
            }
            std::optional<std::string> filePayload = readCompletedResultFile(job.resultFile);
            if (filePayload.has_value()) {
                emitPayload(session, job, partial, filePayload.value(), workerId);
                completedFromFile = true;
                if (!process.wait(1000, exitCode)) {
                    process.terminate(0);
                    process.wait(0, exitCode);
                }
                exited = true;
                break;
            }
            if (process.wait(10, exitCode)) {
                exited = true;
                break;
            }
            if (hasTimedOut(job, started)) {
                result.timedOut = true;
                break;
            }
        }
        if (!exited) {
            process.terminate(0);
            process.wait(0, exitCode);
        }
        std::vector<std::string> finalPayloads;
        readProcessOutput(process, output, error, pending, finalPayloads);
        result.duration = elapsedMilliseconds(started);
        result.exitCode = exitCode;
        result.output = utf8_to_wstring(output);
        result.error = utf8_to_wstring(error);
        if (session->interrupted.load()) {
            result.reason = L"interrupted";
        } else if (result.timedOut) {
            result.reason = L"timeout";
            result.exitCode = 258;
        } else if (completedFromFile) {
            result.reason = L"result_received";
            result.exitCode = 0;
        } else if (exitCode == 0) {
            result.reason = L"ok";
        } else {
            result.reason = L"exit";
        }
        for (const std::string& payload : finalPayloads) {
            emitPayload(session, job, result, payload, workerId);
        }
        return result;
    }
    //=============================================================================
    void
    runDirectLane(const std::shared_ptr<SupervisorSession>& session, uint64 workerId)
    {
        while (!session->interrupted.load()) {
            std::optional<size_t> index = popJob(session, false);
            if (!index.has_value()) {
                return;
            }
            const TestProcessJob& job = session->jobs[index.value()];
            TestProcessResult result = runDirectProcess(session, job, workerId);
            emitProcess(session, job, result, workerId);
            caseCompleted(session);
        }
    }
    //=============================================================================
    TestProcessResult
    reusableFailure(const TestChildProcess& process, Clock::time_point started,
        const std::string& output, const std::string& error, int exitCode, bool timedOut)
    {
        TestProcessResult result = baseResult(process);
        result.duration = elapsedMilliseconds(started);
        result.exitCode = timedOut ? 258 : exitCode;
        result.timedOut = timedOut;
        result.output = utf8_to_wstring(output);
        result.error = utf8_to_wstring(error);
        result.reason = timedOut ? L"timeout" : L"missing_result";
        if (!timedOut && result.exitCode == 0) {
            result.exitCode = 1;
        }
        return result;
    }
    //=============================================================================
    void
    runReusableProcess(
        const std::shared_ptr<SupervisorSession>& session, uint64 workerId, size_t firstIndex)
    {
        TestProcessJob current = session->jobs[firstIndex];
        TestChildProcess process;
        std::wstring launchError;
        if (!process.start(current.executable, current.arguments, true, launchError)) {
            TestProcessResult result;
            result.error = launchError;
            result.reason = L"launch_error";
            trace(session, "worker_launch_error " + wstring_to_utf8(launchError));
            emitProcess(session, current, result, workerId);
            caseCompleted(session);
            return;
        }
        trace(session,
            "worker_start worker=" + std::to_string(workerId)
                + " pid=" + std::to_string(process.pid()));
        std::optional<TestProcessJob> active = current;
        Clock::time_point caseStarted = Clock::now();
        std::string output;
        std::string error;
        std::string pending;
        int exitCode = -1;
        if (!process.writeLine(wstring_to_utf8(active->payload))) {
            process.terminate(0);
        }
        while (!session->interrupted.load()) {
            std::vector<std::string> payloads;
            readProcessOutput(process, output, error, pending, payloads);
            if (!payloads.empty() && active.has_value()) {
                TestProcessResult result = baseResult(process);
                result.duration = elapsedMilliseconds(caseStarted);
                result.reason = L"payload";
                emitPayload(session, active.value(), result, payloads.front(), workerId);
                caseCompleted(session);
                output.clear();
                error.clear();
                active.reset();
                std::optional<size_t> next = popJob(session, true);
                if (next.has_value()) {
                    active = session->jobs[next.value()];
                    caseStarted = Clock::now();
                    if (!process.writeLine(wstring_to_utf8(active->payload))) {
                        break;
                    }
                } else {
                    process.closeInput();
                }
            }
            if (!active.has_value()) {
                if (!process.wait(100, exitCode)) {
                    process.terminate(0);
                }
                break;
            }
            if (process.wait(10, exitCode)) {
                break;
            }
            if (hasTimedOut(active.value(), caseStarted)) {
                process.terminate(0);
                TestProcessResult result
                    = reusableFailure(process, caseStarted, output, error, exitCode, true);
                emitProcess(session, active.value(), result, workerId);
                caseCompleted(session);
                active.reset();
                break;
            }
        }
        if (session->interrupted.load()) {
            process.terminate(0);
            return;
        }
        if (active.has_value()) {
            std::vector<std::string> payloads;
            readProcessOutput(process, output, error, pending, payloads);
            if (!payloads.empty()) {
                TestProcessResult result = baseResult(process);
                result.duration = elapsedMilliseconds(caseStarted);
                result.reason = L"payload";
                emitPayload(session, active.value(), result, payloads.front(), workerId);
            } else {
                TestProcessResult result
                    = reusableFailure(process, caseStarted, output, error, exitCode, false);
                emitProcess(session, active.value(), result, workerId);
            }
            caseCompleted(session);
        }
        trace(session,
            "worker_stop worker=" + std::to_string(workerId)
                + " pid=" + std::to_string(process.pid()));
    }
    //=============================================================================
    void
    runReusableLane(const std::shared_ptr<SupervisorSession>& session, uint64 workerId)
    {
        while (!session->interrupted.load()) {
            std::optional<size_t> first = popJob(session, true);
            if (!first.has_value()) {
                return;
            }
            runReusableProcess(session, workerId, first.value());
        }
    }
    //=============================================================================
    void
    runLane(const std::shared_ptr<SupervisorSession>& session, uint64 workerId, bool reusableFirst)
    {
        if (reusableFirst) {
            runReusableLane(session, workerId);
            runDirectLane(session, workerId);
        } else {
            runDirectLane(session, workerId);
            runReusableLane(session, workerId);
        }
    }
    //=============================================================================
    size_t
    reusableWorkerCount(const SupervisorSession& session, size_t workerCount)
    {
        if (session.reusableQueue.empty()) {
            return 0;
        }
        if (session.directQueue.empty()) {
            return workerCount;
        }
        double reusableWeight = 0;
        double directWeight = 0;
        for (size_t index : session.reusableQueue) {
            reusableWeight += session.jobs[index].weight;
        }
        for (size_t index : session.directQueue) {
            directWeight += session.jobs[index].weight;
        }
        size_t count = static_cast<size_t>(
            std::llround(workerCount * reusableWeight / (reusableWeight + directWeight)));
        return (std::max)(static_cast<size_t>(1), (std::min)(workerCount - 1, count));
    }
    //=============================================================================
    void
    joinThreads(const std::shared_ptr<SupervisorSession>& session)
    {
        std::lock_guard<std::mutex> lock(session->joinMutex);
        if (session->joined) {
            return;
        }
        for (std::thread& thread : session->threads) {
            if (thread.joinable()) {
                thread.join();
            }
        }
        session->joined = true;
    }
    //=============================================================================
    std::shared_ptr<SupervisorSession>
    getSession(uint64 handle)
    {
        std::lock_guard<std::mutex> lock(supervisorMutex);
        auto found = supervisors.find(handle);
        return found == supervisors.end() ? nullptr : found->second;
    }
    //=============================================================================
} // namespace
//=============================================================================
uint64
startTestProcessSupervisor(const std::vector<TestProcessJob>& jobs, size_t workerCount)
{
    auto session = std::make_shared<SupervisorSession>();
    session->jobs = jobs;
    for (size_t index = 0; index < jobs.size(); ++index) {
        if (jobs[index].reusable) {
            session->reusableQueue.push_back(index);
        } else {
            session->directQueue.push_back(index);
        }
    }
    auto priority = [&jobs](size_t left, size_t right) { return jobPriority(jobs, left, right); };
    std::stable_sort(session->reusableQueue.begin(), session->reusableQueue.end(), priority);
    std::stable_sort(session->directQueue.begin(), session->directQueue.end(), priority);
    workerCount = (std::min)(workerCount, jobs.size());
    {
        std::lock_guard<std::mutex> lock(supervisorMutex);
        session->handle = nextSupervisorHandle++;
        supervisors[session->handle] = session;
    }
    size_t reusableCount = workerCount == 0 ? 0 : reusableWorkerCount(*session, workerCount);
    trace(session,
        "start jobs=" + std::to_string(jobs.size()) + " workers=" + std::to_string(workerCount)
            + " reusable_workers=" + std::to_string(reusableCount));
    for (size_t index = 0; index < workerCount; ++index) {
        bool reusableFirst = index < reusableCount;
        uint64 workerId = static_cast<uint64>(index + 1);
        session->threads.emplace_back(
            [session, workerId, reusableFirst]() { runLane(session, workerId, reusableFirst); });
    }
    return session->handle;
}
//=============================================================================
std::vector<TestProcessEvent>
pollTestProcessSupervisor(uint64 handle, uint64 timeoutMs)
{
    std::shared_ptr<SupervisorSession> session = getSession(handle);
    if (session == nullptr) {
        return {};
    }
    std::unique_lock<std::mutex> lock(session->eventMutex);
    if (session->events.empty() && session->completed.load() < session->jobs.size()) {
        session->eventCondition.wait_for(lock, std::chrono::milliseconds(timeoutMs));
    }
    std::vector<TestProcessEvent> events;
    while (!session->events.empty()) {
        events.push_back(std::move(session->events.front()));
        session->events.pop_front();
    }
    bool complete = session->completed.load() >= session->jobs.size();
    lock.unlock();
    if (complete) {
        joinThreads(session);
        lock.lock();
        if (session->events.empty() && !session->doneEmitted) {
            TestProcessEvent done;
            done.eventType = L"done";
            done.result.exitCode = 0;
            done.result.reason.clear();
            done.sequence = session->nextSequence.fetch_add(1);
            events.push_back(std::move(done));
            session->doneEmitted = true;
            trace(session, "done");
        }
    }
    return events;
}
//=============================================================================
void
stopTestProcessSupervisor(uint64 handle)
{
    std::shared_ptr<SupervisorSession> session;
    {
        std::lock_guard<std::mutex> lock(supervisorMutex);
        auto found = supervisors.find(handle);
        if (found == supervisors.end()) {
            return;
        }
        session = found->second;
        supervisors.erase(found);
    }
    if (session->completed.load() < session->jobs.size()) {
        trace(session,
            "stop interrupted completed=" + std::to_string(session->completed.load()) + "/"
                + std::to_string(session->jobs.size()));
        session->interrupted.store(true);
        session->eventCondition.notify_all();
    }
    joinThreads(session);
}
//=============================================================================
uint64
stopAllTestProcessSupervisors()
{
    std::vector<uint64> handles;
    {
        std::lock_guard<std::mutex> lock(supervisorMutex);
        handles.reserve(supervisors.size());
        for (const auto& supervisor : supervisors) {
            handles.push_back(supervisor.first);
        }
    }
    for (uint64 handle : handles) {
        stopTestProcessSupervisor(handle);
    }
    return static_cast<uint64>(handles.size());
}
//=============================================================================
} // namespace Nelson::TestsManager
//=============================================================================
