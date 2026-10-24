%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function results = emptyNativeResults(count)
  result = struct('exit_code', 0, 'timed_out', false, 'duration', 0, ...
    'stdout', '', 'stderr', '', 'kind', '', 'index', 0, 'metadata', '', ...
    'job_id', '', 'pid', 0, 'timeout', 0, 'reason', '', 'executable', '', ...
    'process_policy', '', 'process_arguments', '');
  results = repmat(result, 1, count);
end
