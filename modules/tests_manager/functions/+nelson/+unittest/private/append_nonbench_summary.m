%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suite = append_nonbench_summary(test_suite, status)
  switch status %#ok<NLS0008>
    case 'Fail'
      test_suite.errors = test_suite.errors + 1;
    case 'Pass'
      test_suite.passed = test_suite.passed + 1;
    case {'Skip', 'Interactive', 'No display'}
      test_suite.disabled = test_suite.disabled + 1;
  end
end
%=============================================================================
