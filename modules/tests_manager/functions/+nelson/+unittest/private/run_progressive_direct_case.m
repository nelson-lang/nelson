%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function testCase = run_progressive_direct_case(directCase, nativeResult, abortedCode)
  testCase = run_test_batch_from_native(directCase, nativeResult, abortedCode);
end
%=============================================================================
