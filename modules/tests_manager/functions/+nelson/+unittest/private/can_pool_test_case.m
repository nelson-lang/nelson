%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = can_pool_test_case(test_case)
  if isfield(test_case, 'worker_pool_eligible') && ~isempty(test_case.worker_pool_eligible)
    tf = logical(test_case.worker_pool_eligible);
    return
  end
  tf = compute_worker_pool_eligibility(test_case);
end
%=============================================================================
