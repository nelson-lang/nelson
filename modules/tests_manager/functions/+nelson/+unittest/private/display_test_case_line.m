%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function display_test_case_line(testCase, indexTest, nbTotalTests, fmtStart)
  startChars = sprintf(fmtStart, indexTest, nbTotalTests, testCase.name);
  elapsedChars = sprintf('[%8.3fs]', max(0, double(testCase.time)));
  statusChars = [getStatusCharacter(testCase.status), elapsedChars];
  nb_spaces = max(1, 80 - length(startChars) - length(statusChars) + 1);
  fprintf(stdout, '%s%s%s%s', startChars, blanks(nb_spaces), statusChars, newline);
  displayTestCaseFail(testCase)
end
%=============================================================================
