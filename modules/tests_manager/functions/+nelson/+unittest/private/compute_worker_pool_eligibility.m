%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = compute_worker_pool_eligibility(test_case)
  tf = ~test_case.skip ...
    && isfield(test_case.options, 'reuse_process') && test_case.options.reuse_process ...
    && can_pool_test_case_mode(test_case.options) ...
    && can_pool_test_case_resources(test_case.options);
end
%=============================================================================
