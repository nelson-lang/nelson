%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [tests, bugs] = split_bug_test_cases(testCases)
  tests = [];
  bugs = [];
  for k = 1:length(testCases)
    if is_bug_test_case(testCases(k))
      bugs = [bugs; testCases(k)];
    else
      tests = [tests; testCases(k)];
    end
  end
end
%=============================================================================
function tf = is_bug_test_case(testCase)
  [~, name, ~] = fileparts(testCase.filename); %#ok<NLS0004>
  tf = startsWith(name, 'bug_');
end
%=============================================================================
