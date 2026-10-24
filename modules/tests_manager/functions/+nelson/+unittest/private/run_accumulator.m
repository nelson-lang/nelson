%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [status, message, nativeResults] = run_accumulator(accumulator, nbWorkers)
  nbAccumulator = length(accumulator);
  status = zeros(1, nbAccumulator);
  message = cell(1, nbAccumulator);
  nativeResults = emptyNativeResults(nbAccumulator);
  if use_inline_unittest_runner()
    [status, message, nativeResults] = run_accumulator_inline(accumulator);
    return
  end
  if ~canUseNativeRunner()
    error('Nelson:tests_manager:supervisorUnavailable', 'Native test process supervisor unavailable.');
  end
  [executables, processArguments, nativeTimeout, nativeIndex, message] = ...
    collectNativeRunInputs(accumulator, message);
  if ~isempty(nativeIndex)
    nativeJobs = run_process_jobs(executables, processArguments, nativeTimeout, ...
      run_accumulator_job_kind(nativeIndex), nativeIndex, ...
      run_accumulator_job_metadata(accumulator, nativeIndex));
    nativeSubset = run_processes(nativeJobs, nbWorkers);
    for i = 1:length(nativeSubset)
      nativeResults(nativeIndex(i)) = nativeSubset(i);
      status(nativeIndex(i)) = nativeSubset(i).exit_code;
      message{nativeIndex(i)} = nativeRunMessage(nativeSubset(i));
    end
  end
  message = fillEmptyRunMessages(message);
end
