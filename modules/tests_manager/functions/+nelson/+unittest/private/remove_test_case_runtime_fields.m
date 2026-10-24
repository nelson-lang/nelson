%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_cases = remove_test_case_runtime_fields(test_cases)
  if isempty(test_cases)
    return
  end
  fields = {'command', 'executable', 'process_arguments', 'outputfile', ...
    'redirect_err', 'command_filename', 'launcher_timeout', 'runfile_result', ...
    'res_cmd', 'skip', 'pool_index', 'pool_missing_result', 'test_order_index', ...
    'worker_pool_eligible', 'worker_pool_failure'};
  for k = 1:length(fields)
    if isfield(test_cases, fields{k})
      test_cases = rmfield(test_cases, fields{k});
    end
  end
end
%=============================================================================
