%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function display_progressive_case(testCase, indexTest, nbTotalTests, fmtStart)
  display_test_case_line(testCase, indexTest, nbTotalTests, fmtStart);
  handle_interrupted_test(testCase);
  display_skip_reason(testCase);
end
%=============================================================================
