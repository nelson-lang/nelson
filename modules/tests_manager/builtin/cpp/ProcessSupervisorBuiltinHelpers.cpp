//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ProcessSupervisorBuiltinHelpers.hpp"
//=============================================================================
namespace Nelson::TestsManagerGateway {
//=============================================================================
namespace {
    //=============================================================================
    bool
    hasField(const ArrayOf& value, const std::string& field)
    {
        stringVector fields = value.fieldNames();
        return std::find(fields.begin(), fields.end(), field) != fields.end();
    }
    //=============================================================================
    std::vector<double>
    indexValues(size_t count)
    {
        std::vector<double> values(count);
        for (size_t index = 0; index < count; ++index) {
            values[index] = static_cast<double>(index + 1);
        }
        return values;
    }
    //=============================================================================
    wstringVector
    stringValues(size_t count, const std::wstring& value = L"")
    {
        return wstringVector(count, value);
    }
    //=============================================================================
    std::vector<double>
    doubleVector(const ArrayOf& jobs, const std::string& field, size_t count,
        const std::vector<double>& defaults)
    {
        if (jobs.isField(field)) {
            std::vector<double> values = jobs.field(field).asDoubleVector();
            if (values.size() == count) {
                return values;
            }
        }
        return defaults;
    }
    //=============================================================================
    wstringVector
    stringFieldVector(const ArrayOf& jobs, const std::string& field, size_t count,
        const std::wstring& defaultValue = L"")
    {
        if (jobs.isField(field)) {
            wstringVector values = jobs.field(field).asWideStringVector(true);
            if (values.size() == count) {
                return values;
            }
        }
        return stringValues(count, defaultValue);
    }
    //=============================================================================
    std::vector<uint64>
    timeoutVector(ArrayOf value, size_t count)
    {
        if (!value.isNumeric() || !value.isVector() || value.elementCount() != count) {
            Error(L"Nelson:tests_manager:invalidTimeouts",
                L"timeouts must be a numeric vector matching the number of jobs.");
        }
        value.promoteType(NLS_UINT64);
        const uint64* pointer = static_cast<const uint64*>(value.dataPointer());
        return std::vector<uint64>(pointer, pointer + count);
    }
    //=============================================================================
    std::vector<uint64>
    recordTimeouts(const ArrayOf& jobs)
    {
        std::string field = hasField(jobs, "timeouts") ? "timeouts"
            : hasField(jobs, "timeout")                ? "timeout"
                                                       : "launcher_timeout";
        if (!hasField(jobs, field)) {
            Error(L"Nelson:tests_manager:jobRecordMissingTimeout",
                L"job records must contain timeout, timeouts or launcher_timeout.");
        }
        ArrayOfVector values = jobs.fieldAsList(field);
        std::vector<uint64> result;
        result.reserve(values.size());
        for (const ArrayOf& value : values) {
            result.push_back(static_cast<uint64>(value.asDoubleScalar()));
        }
        return result;
    }
    //=============================================================================
    wstringVector
    recordStrings(const ArrayOf& jobs, const std::string& field)
    {
        ArrayOfVector values = jobs.fieldAsList(field);
        wstringVector result;
        result.reserve(values.size());
        for (const ArrayOf& value : values) {
            result.push_back(value.asWideString());
        }
        return result;
    }
    //=============================================================================
    std::vector<double>
    recordDoubles(const ArrayOf& jobs, const std::string& field)
    {
        ArrayOfVector values = jobs.fieldAsList(field);
        std::vector<double> result;
        result.reserve(values.size());
        for (const ArrayOf& value : values) {
            result.push_back(value.asDoubleScalar());
        }
        return result;
    }
    //=============================================================================
    wstringVector
    recordStringsOr(const ArrayOf& jobs, const std::string& field, size_t count,
        const std::wstring& defaultValue = L"")
    {
        return hasField(jobs, field) ? recordStrings(jobs, field)
                                     : stringValues(count, defaultValue);
    }
    //=============================================================================
    std::vector<double>
    recordDoublesOr(const ArrayOf& jobs, const std::string& field, size_t count,
        const std::vector<double>& defaults)
    {
        return hasField(jobs, field) ? recordDoubles(jobs, field) : defaults;
    }
    //=============================================================================
    std::wstring
    defaultJobId(const std::wstring& kind, double index)
    {
        return (kind.empty() ? L"job" : kind) + L"_" + std::to_wstring(static_cast<uint64>(index));
    }
    //=============================================================================
    void
    fillJobIds(
        wstringVector& values, const wstringVector& kinds, const std::vector<double>& indices)
    {
        for (size_t index = 0; index < values.size(); ++index) {
            if (values[index].empty()) {
                values[index] = defaultJobId(kinds[index], indices[index]);
            }
        }
    }
    //=============================================================================
    std::vector<TestsManager::TestProcessJob>
    makeJobs(const wstringVector& executables, const wstringVector& arguments,
        const std::vector<uint64>& timeouts, const wstringVector& kinds,
        const std::vector<double>& indices, const wstringVector& metadata, wstringVector jobIds,
        const std::vector<double>& schedule, const std::vector<double>& weights,
        const std::vector<double>& reusable, const wstringVector& payloads,
        const wstringVector& launchers, const wstringVector& resultFiles,
        const std::vector<double>& attempts)
    {
        fillJobIds(jobIds, kinds, indices);
        std::vector<TestsManager::TestProcessJob> jobs;
        jobs.reserve(executables.size());
        for (size_t index = 0; index < executables.size(); ++index) {
            TestsManager::TestProcessJob job;
            job.executable = executables[index];
            job.arguments = arguments[index];
            job.timeout = timeouts[index];
            job.kind = kinds[index];
            job.index = indices[index];
            job.metadata = metadata[index];
            job.jobId = jobIds[index];
            job.scheduleOrder = schedule[index];
            job.weight = weights[index] > 0 ? weights[index] : 1;
            job.reusable = reusable[index] != 0;
            job.payload = payloads[index];
            job.launcher = launchers[index];
            job.resultFile = resultFiles[index];
            job.attempt = attempts[index];
            job.inputPosition = index;
            jobs.push_back(std::move(job));
        }
        return jobs;
    }
    //=============================================================================
    std::vector<TestsManager::TestProcessJob>
    jobsFromScalarStruct(const ArrayOf& jobs)
    {
        if (!jobs.isScalar() || !jobs.isField("executables") || !jobs.isField("process_arguments")
            || !jobs.isField("timeouts")) {
            Error(L"Nelson:tests_manager:jobsMissingFields",
                L"jobs must contain executables, process_arguments and timeouts.");
        }
        wstringVector executables = jobs.field("executables").asWideStringVector(true);
        size_t count = executables.size();
        wstringVector arguments = stringFieldVector(jobs, "process_arguments", count);
        std::vector<uint64> timeouts = timeoutVector(jobs.field("timeouts"), count);
        wstringVector kinds = stringFieldVector(jobs, "kind", count, L"direct");
        std::vector<double> indices = doubleVector(jobs, "index", count, indexValues(count));
        wstringVector metadata = stringFieldVector(jobs, "metadata", count);
        wstringVector ids = stringFieldVector(jobs, "job_id", count);
        std::vector<double> schedule
            = doubleVector(jobs, "schedule_order", count, indexValues(count));
        std::vector<double> weights
            = doubleVector(jobs, "weight", count, std::vector<double>(count, 1));
        std::vector<double> reusable
            = doubleVector(jobs, "reusable", count, std::vector<double>(count, 0));
        wstringVector payloads = stringFieldVector(jobs, "payload", count);
        wstringVector launchers = stringFieldVector(jobs, "launcher", count);
        wstringVector resultFiles = stringFieldVector(jobs, "result_file", count);
        std::vector<double> attempts
            = doubleVector(jobs, "attempt", count, std::vector<double>(count, 1));
        return makeJobs(executables, arguments, timeouts, kinds, indices, metadata, ids, schedule,
            weights, reusable, payloads, launchers, resultFiles, attempts);
    }
    //=============================================================================
    std::vector<TestsManager::TestProcessJob>
    jobsFromRecords(const ArrayOf& jobs)
    {
        if (!hasField(jobs, "executable") || !hasField(jobs, "process_arguments")) {
            Error(L"Nelson:tests_manager:invalidJobRecord",
                L"job records must contain executable and process_arguments.");
        }
        size_t count = static_cast<size_t>(jobs.elementCount());
        wstringVector executables = recordStrings(jobs, "executable");
        wstringVector arguments = recordStrings(jobs, "process_arguments");
        std::vector<uint64> timeouts = recordTimeouts(jobs);
        wstringVector kinds = recordStringsOr(jobs, "kind", count, L"pool");
        std::vector<double> indices = recordDoublesOr(jobs, "index", count, indexValues(count));
        wstringVector metadata = hasField(jobs, "metadata") ? recordStrings(jobs, "metadata")
            : hasField(jobs, "command_filename") ? recordStrings(jobs, "command_filename")
                                                 : stringValues(count);
        wstringVector ids = recordStringsOr(jobs, "job_id", count);
        std::vector<double> schedule
            = recordDoublesOr(jobs, "schedule_order", count, indexValues(count));
        std::vector<double> weights
            = recordDoublesOr(jobs, "weight", count, std::vector<double>(count, 1));
        std::vector<double> reusable
            = recordDoublesOr(jobs, "reusable", count, std::vector<double>(count, 0));
        wstringVector payloads = recordStringsOr(jobs, "payload", count);
        wstringVector launchers = recordStringsOr(jobs, "launcher", count);
        wstringVector resultFiles = recordStringsOr(jobs, "result_file", count);
        std::vector<double> attempts
            = recordDoublesOr(jobs, "attempt", count, std::vector<double>(count, 1));
        return makeJobs(executables, arguments, timeouts, kinds, indices, metadata, ids, schedule,
            weights, reusable, payloads, launchers, resultFiles, attempts);
    }
    //=============================================================================
    ArrayOf
    eventStruct(const TestsManager::TestProcessEvent& event)
    {
        stringVector fields = { "event_type", "exit_code", "timed_out", "duration", "stdout",
            "stderr", "kind", "index", "case_index", "metadata", "job_id", "pid", "timeout",
            "reason", "process_policy", "executable", "process_arguments", "payload", "sequence",
            "worker_id", "weight", "reusable", "launcher", "result_file", "attempt" };
        ArrayOfVector values;
        values.push_back(ArrayOf::characterArray(event.eventType));
        values.push_back(ArrayOf::doubleScalar(event.result.exitCode));
        values.push_back(ArrayOf::logicalScalar(event.result.timedOut));
        values.push_back(ArrayOf::doubleScalar(static_cast<double>(event.result.duration)));
        values.push_back(ArrayOf::characterArray(event.result.output));
        values.push_back(ArrayOf::characterArray(event.result.error));
        values.push_back(ArrayOf::characterArray(event.job.kind));
        values.push_back(ArrayOf::doubleScalar(event.job.index));
        values.push_back(ArrayOf::doubleScalar(event.job.index));
        values.push_back(ArrayOf::characterArray(event.job.metadata));
        values.push_back(ArrayOf::characterArray(event.job.jobId));
        values.push_back(ArrayOf::doubleScalar(static_cast<double>(event.result.pid)));
        values.push_back(ArrayOf::doubleScalar(static_cast<double>(event.job.timeout)));
        values.push_back(ArrayOf::characterArray(event.result.reason));
        values.push_back(ArrayOf::characterArray(event.result.processPolicy));
        values.push_back(ArrayOf::characterArray(event.job.executable));
        values.push_back(ArrayOf::characterArray(event.job.arguments));
        values.push_back(ArrayOf::characterArray(event.payload));
        values.push_back(ArrayOf::doubleScalar(static_cast<double>(event.sequence)));
        values.push_back(ArrayOf::doubleScalar(static_cast<double>(event.workerId)));
        values.push_back(ArrayOf::doubleScalar(event.job.weight));
        values.push_back(ArrayOf::logicalScalar(event.job.reusable));
        values.push_back(ArrayOf::characterArray(event.job.launcher));
        values.push_back(ArrayOf::characterArray(event.job.resultFile));
        values.push_back(ArrayOf::doubleScalar(event.job.attempt));
        return ArrayOf::structArray(fields, values);
    }
    //=============================================================================
    ArrayOf
    doubleCell(const std::vector<double>& values)
    {
        ArrayOfVector row;
        for (double value : values) {
            row.push_back(ArrayOf::doubleScalar(value));
        }
        ArrayOfMatrix matrix;
        matrix.push_back(row);
        return ArrayOf::cellArray(matrix);
    }
    //=============================================================================
    ArrayOf
    logicalCell(const std::vector<bool>& values)
    {
        ArrayOfVector row;
        for (bool value : values) {
            row.push_back(ArrayOf::logicalScalar(value));
        }
        ArrayOfMatrix matrix;
        matrix.push_back(row);
        return ArrayOf::cellArray(matrix);
    }
    //=============================================================================
} // namespace
//=============================================================================
std::vector<TestsManager::TestProcessJob>
processSupervisorJobsFromInput(const ArrayOfVector& argIn)
{
    const ArrayOf& jobs = argIn[0];
    if (!jobs.isStruct()) {
        Error(L"Nelson:tests_manager:invalidJobs", L"jobs must be a struct.");
    }
    return hasField(jobs, "executable") ? jobsFromRecords(jobs) : jobsFromScalarStruct(jobs);
}
//=============================================================================
size_t
processSupervisorWorkerCount(const ArrayOfVector& argIn, size_t jobCount)
{
    size_t count = jobCount;
    if (argIn.size() == 2) {
        double requested = argIn[1].asDoubleScalar();
        if (requested < 1) {
            Error(L"Nelson:tests_manager:invalidWorkerCount", L"worker count must be positive.");
        }
        count = (std::min)(jobCount, static_cast<size_t>(requested));
    }
    return count;
}
//=============================================================================
ArrayOf
processSupervisorEventsToStruct(const std::vector<TestsManager::TestProcessEvent>& events)
{
    ArrayOfVector row;
    row.reserve(events.size());
    for (const TestsManager::TestProcessEvent& event : events) {
        row.push_back(eventStruct(event));
    }
    ArrayOfMatrix matrix;
    matrix.push_back(row);
    return ArrayOf::cellArray(matrix);
}
//=============================================================================
ArrayOf
processSupervisorResultsToStruct(const std::vector<TestsManager::TestProcessJob>& jobs,
    const std::vector<TestsManager::TestProcessResult>& results)
{
    std::vector<double> exitCodes, durations, indices, pids, timeouts;
    std::vector<bool> timedOut;
    wstringVector outputs, errors, kinds, metadata, ids, reasons, policies, executables, arguments;
    for (size_t index = 0; index < jobs.size(); ++index) {
        const auto& job = jobs[index];
        const auto& result = results[index];
        exitCodes.push_back(result.exitCode);
        timedOut.push_back(result.timedOut);
        durations.push_back(static_cast<double>(result.duration));
        outputs.push_back(result.output);
        errors.push_back(result.error);
        kinds.push_back(job.kind);
        indices.push_back(job.index);
        metadata.push_back(job.metadata);
        ids.push_back(job.jobId);
        pids.push_back(static_cast<double>(result.pid));
        timeouts.push_back(static_cast<double>(job.timeout));
        reasons.push_back(result.reason);
        policies.push_back(result.processPolicy);
        executables.push_back(job.executable);
        arguments.push_back(job.arguments);
    }
    stringVector fields = { "exit_code", "timed_out", "duration", "stdout", "stderr", "kind",
        "index", "metadata", "job_id", "pid", "timeout", "reason", "process_policy", "executable",
        "process_arguments" };
    ArrayOfVector values;
    values.push_back(doubleCell(exitCodes));
    values.push_back(logicalCell(timedOut));
    values.push_back(doubleCell(durations));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(outputs));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(errors));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(kinds));
    values.push_back(doubleCell(indices));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(metadata));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(ids));
    values.push_back(doubleCell(pids));
    values.push_back(doubleCell(timeouts));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(reasons));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(policies));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(executables));
    values.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(arguments));
    return ArrayOf::structArray(fields, values);
}
//=============================================================================
} // namespace Nelson::TestsManagerGateway
//=============================================================================
