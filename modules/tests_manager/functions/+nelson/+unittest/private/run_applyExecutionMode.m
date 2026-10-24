%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cleanup = run_applyExecutionMode(executionMode)
  if isempty(executionMode)
    executionMode = 'auto';
  end
  executionMode = lower(executionMode);
  if ~any(strcmp(executionMode, {'auto', 'inline', 'process'}))
    error('Nelson:tests_manager:unsupportedExecutionMode', ...
      'Unsupported ''ExecutionMode'' value: %s (use ''auto'', ''inline'', or ''process'').', ...
      executionMode);
  end
  previous = getenv('NELSON_UNITTEST_EXECUTION_MODE');
  setenv('NELSON_UNITTEST_EXECUTION_MODE', upper(executionMode));
  cleanup = onCleanup(@() setenv('NELSON_UNITTEST_EXECUTION_MODE', previous));
end
%=============================================================================
