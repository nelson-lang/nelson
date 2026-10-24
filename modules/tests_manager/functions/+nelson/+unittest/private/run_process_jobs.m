%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function jobs = run_process_jobs(executables, processArguments, timeouts, kind, index, metadata, jobId)
  if nargin < 7
    jobId = run_process_default_job_id(kind, index);
  end
  jobs = struct();
  jobs.executables = executables;
  jobs.process_arguments = processArguments;
  jobs.timeouts = uint64(timeouts);
  jobs.kind = kind;
  jobs.index = index;
  jobs.metadata = metadata;
  jobs.job_id = jobId;
end
%=============================================================================
function jobId = run_process_default_job_id(kind, index)
  jobId = cell(length(index), 1);
  for k = 1:length(index)
    jobId{k} = [kind{k}, '_', int2str(index(k))];
  end
end
%=============================================================================
