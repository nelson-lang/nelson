%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [test_cases_updated, indexTest] = display_test_batch(...
    test_cases_updated, accumulator, indexTest, nbTotalTests, fmtStart)
  for i = 1:length(accumulator)
    test_cases_updated = [test_cases_updated; accumulator(i)];
    display_test_case_line(accumulator(i), indexTest, nbTotalTests, fmtStart);
    handle_interrupted_test(accumulator(i));
    display_skip_reason(accumulator(i));
    indexTest = indexTest + 1;
  end
end
%=============================================================================
