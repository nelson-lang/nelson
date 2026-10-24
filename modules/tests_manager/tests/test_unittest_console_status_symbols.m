%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
privateDir = [nelsonroot(), '/modules/tests_manager/functions/+nelson/+unittest/private'];
addpath(privateDir);
asserts.isequal(getUnicodeStatusCharacter('Pass'), '🟢');
asserts.isequal(getUnicodeStatusCharacter('Fail'), '🔴');
asserts.isequal(getUnicodeStatusCharacter('Skip'), '🟡');
asserts.isequal(getUnicodeStatusCharacter('3.2 s'), '3.2 s');
testCase = struct('name', 'duration_case', 'time', 9.8, ...
  'status', 'Pass', 'msg', '');
line = evalc('display_test_case_line(testCase, 1, 1, test_start_format(1))');
asserts.istrue(contains(line, '[   9.800s]'));
%=============================================================================
