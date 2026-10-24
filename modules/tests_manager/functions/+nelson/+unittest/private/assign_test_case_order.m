%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = assign_test_case_order(test_cases)
  for k = 1:length(test_cases)
    test_cases(k).test_order_index = k;
  end
end
%=============================================================================
