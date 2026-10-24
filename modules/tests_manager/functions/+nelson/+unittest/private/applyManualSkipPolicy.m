%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = applyManualSkipPolicy(test_case)
  if (test_case.options.not_fixed)
    test_case = markSkipped(test_case, 'Skip');
  end
  if (test_case.options.interactive_test)
    test_case = markSkipped(test_case, 'Interactive');
  end
end
%=============================================================================
