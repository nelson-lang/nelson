%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function jobs = progressive_append_job(jobs, testCase, index, attempt, ...
    executable, processArguments, payload, kind, launcher, reusable)
  jobs.executables = [jobs.executables, cellstr(executable)];
  jobs.process_arguments = [jobs.process_arguments, cellstr(processArguments)];
  jobs.timeouts = [jobs.timeouts, testCase.launcher_timeout];
  jobs.kind = [jobs.kind, cellstr(kind)];
  jobs.index = [jobs.index, index];
  jobs.metadata = [jobs.metadata, cellstr(testCase.filename)];
  jobId = [kind, '_', int2str(index), '_attempt_', int2str(attempt)];
  jobs.job_id = [jobs.job_id, cellstr(jobId)];
  jobs.schedule_order = [jobs.schedule_order, index];
  jobs.weight = [jobs.weight, test_case_weight(testCase)];
  jobs.reusable = [jobs.reusable, double(reusable)];
  jobs.payload = [jobs.payload, cellstr(payload)];
  jobs.launcher = [jobs.launcher, cellstr(launcher)];
  jobs.result_file = [jobs.result_file, cellstr(testCase.outputfile)];
  jobs.attempt = [jobs.attempt, attempt];
end
%=============================================================================
