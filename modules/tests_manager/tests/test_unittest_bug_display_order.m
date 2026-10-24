%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_bug_display_order_', createGUID()];
testFile = [base, '_test_sequential.m'];
bugFile = [fileparts(base, 'path'), '/bug_unittest_display_order_', createGUID(), '.m'];
cleanupTest = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupBug = onCleanup(@() evalc(removeCommand(bugFile))); %#ok<NLS0004>

filewrite(testFile, {'% <--SEQUENTIAL TEST REQUIRED-->'; 'asserts.istrue(true);'});
filewrite(bugFile, {'asserts.istrue(true);'});

output = evalc('status = test_run({testFile, bugFile});');
asserts.istrue(status);
testPosition = strfind(output, 'test_unittest_bug_display_order');
bugPosition = strfind(output, 'bug_unittest_display_order');
asserts.istrue(~isempty(testPosition));
asserts.istrue(~isempty(bugPosition));
asserts.istrue(testPosition(1) < bugPosition(1));
%=============================================================================
