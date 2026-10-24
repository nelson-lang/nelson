%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNativeRunner = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
base = [[tempdir(), ''], 'test_unittest_retry_timeout_', createGUID()];
testFile = [base, '_test_timeout_then_pass.m'];
markerFile = [base, '_marker.txt'];
cleanupTest = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile))); %#ok<NLS0004>

filewrite(testFile, {
  ['marker = ''', markerFile, ''';']
  'if ~isfile(marker)'
  '  filewrite(marker, ''timeout once'');'
  '  pause(20);'
  'end'
  'asserts.istrue(true);'
  });

suite = nelson.unittest.discover(testFile, 'Kind', 'test');
testPlan = nelson.unittest.plan(suite, 'Workers', 1);
results = nelson.unittest.run(testPlan, 'Retry', 1, 'RetryOnlyOn', 'timeout', 'Timeout', 8);
asserts.istrue(results.status);
asserts.isequal(results.summary.timeout, 0);
asserts.isequal(results.cases(1).outcome, 'passed');
asserts.isequal(results.cases(1).attempts, 2);
%=============================================================================
