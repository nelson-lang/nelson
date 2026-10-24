%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function value = test_case_weight(testCase)
  value = 1;
  if isfield(testCase, 'weight') && isnumeric(testCase.weight) ...
      && isscalar(testCase.weight) && testCase.weight > 0
    value = double(testCase.weight);
  end
end
%=============================================================================
