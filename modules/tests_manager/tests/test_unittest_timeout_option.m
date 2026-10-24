%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNativeRunner = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
base = [[tempdir(), ''], 'test_unittest_timeout_option_', createGUID()];
testFile = [base, '_test_timeout_env.m'];
markerFile = [base, '_timeout.txt'];
cleanupTest = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile))); %#ok<NLS0004>

filewrite(testFile, {
  ['filewrite(''', markerFile, ''', ''done'');']
  'asserts.istrue(true);'
  });

suite = nelson.unittest.discover(testFile, 'Kind', 'test');
testPlan = nelson.unittest.plan(suite, 'Workers', 1);
results = nelson.unittest.run(testPlan, 'Timeout', 180);
asserts.istrue(results.status);
asserts.isequal(fileread(markerFile), 'done');
asserts.isequal(results.raw.tests_list.test_cases_list.timeout, 180);
%=============================================================================
