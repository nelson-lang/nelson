%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = configure_test_case_launchers(test_cases)
  tests_manager_trace('launcher', ['configure count=', int2str(length(test_cases))]);
  for k = 1:length(test_cases)
    if ~test_cases(k).skip && isempty(test_cases(k).executable)
      test_cases(k) = configure_test_case_launcher(test_cases(k));
      tests_manager_trace('launcher', ['configured index=', int2str(k)]);
    end
  end
  tests_manager_trace('launcher', 'ready');
end
%=============================================================================
