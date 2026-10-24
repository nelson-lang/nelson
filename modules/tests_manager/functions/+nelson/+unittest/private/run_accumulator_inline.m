%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [status, message, nativeResults] = run_accumulator_inline(accumulator)
  nbAccumulator = length(accumulator);
  status = zeros(1, nbAccumulator);
  message = cell(1, nbAccumulator);
  nativeResults = emptyNativeResults(nbAccumulator);
  [nativeResults.runfile_result] = deal([]);
  for i = 1:nbAccumulator
    if strcmp(accumulator(i).command, 'echo skip')
      message{i} = '';
      continue
    end
    if ~isfile(accumulator(i).filename)
      error('Nelson:tests_manager:fileDoesNotExist', [_('An existing filename expected.'), ' ', accumulator(i).filename]);
    end
    resetGraphicsIfRequested();
    tests_manager_trace('inline', ['run ', accumulator(i).filename]);
    result = worker_pool_safe_runfile(...
      accumulator(i).filename, true, accumulator(i).options.mpi_mode);
    resetGraphicsIfRequested();
    tests_manager_trace('inline', ['complete ', accumulator(i).filename]);
    nativeResults(i).exit_code = result.r;
    nativeResults(i).timed_out = false;
    nativeResults(i).duration = result.timing;
    nativeResults(i).stdout = result.stdout;
    nativeResults(i).stderr = '';
    nativeResults(i).kind = 'inline';
    nativeResults(i).index = i;
    nativeResults(i).metadata = accumulator(i).filename;
    nativeResults(i).job_id = ['inline_', int2str(i)];
    nativeResults(i).timeout = accumulator(i).launcher_timeout;
    nativeResults(i).reason = inline_reason(result.r);
    nativeResults(i).process_policy = 'inline-runfile';
    nativeResults(i).process_arguments = ...
      '--no-user-modules --no-ipc --without-file-watcher -- --no-audio --without-python --without-julia --inline';
    nativeResults(i).runfile_result = result;
    status(i) = result.r;
    message{i} = nativeRunMessage(nativeResults(i));
  end
end
%=============================================================================
function resetGraphicsIfRequested()
  if isTruthyEnvironmentValue('NELSON_UNITTEST_RESET_GRAPHICS') ...
      && exist('close', 'builtin') ~= 0
    close('all');
  end
end
%=============================================================================
function reason = inline_reason(status)
  if status == 0
    reason = 'ok';
  else
    reason = 'exit';
  end
end
%=============================================================================
