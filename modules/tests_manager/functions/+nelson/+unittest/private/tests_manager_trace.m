%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tests_manager_trace(scope, message)
  if strcmp(getenv('NELSON_UNITTEST_TRACE'), '1') ...
      || strcmp(getenv('NELSON_UNITTEST_TRACE_PROGRESSIVE'), '1')
    verbose = strcmp(getenv('NELSON_UNITTEST_TRACE_VERBOSE'), '1');
    detailedScope = strcmp(scope, 'jobs') || strcmp(scope, 'launcher') ...
      || strcmp(scope, 'events');
    if detailedScope && ~verbose
      return
    end
    fprintf(stderr(), '[tests_manager %s] %s%s', scope, message, newline());
  end
end
%=============================================================================
