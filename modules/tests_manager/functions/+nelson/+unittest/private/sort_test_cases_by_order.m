%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = sort_test_cases_by_order(test_cases)
  if isempty(test_cases) || ~isfield(test_cases, 'test_order_index')
    return
  end
  [~, order] = sort([test_cases.test_order_index]);
  test_cases = test_cases(order);
end
%=============================================================================
