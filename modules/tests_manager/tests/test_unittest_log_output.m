%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_log_output_', createGUID()];
testFile = [base, '_test_stdout.m'];
logDir = [base, '_logs'];
cleanupFile = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>

filewrite(testFile, {
  'disp(''captured stdout from test'');'
  'asserts.istrue(true);'
  });

suite = nelson.unittest.discover(testFile, 'Kind', 'test');
testPlan = nelson.unittest.plan(suite, 'Workers', 1);
results = nelson.unittest.run(testPlan, 'LogDir', logDir);
asserts.istrue(results.status);
asserts.istrue(contains(results.cases(1).stdout, 'captured stdout from test'));
asserts.isequal(length(results.cases(1).artifacts), 1);
asserts.istrue(isfile(results.cases(1).artifacts{1}));

logged = jsondecode(results.cases(1).artifacts{1}, '-file');
asserts.isequal(logged.id, results.cases(1).id);
asserts.istrue(contains(logged.stdout, 'captured stdout from test'));
rmdir(logDir, 's');
%=============================================================================
