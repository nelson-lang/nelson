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
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_retry_', createGUID()];
testFile = [base, '_test_flaky.m'];
markerFile = [base, '_marker.txt'];
%#ok<NLS0004>
cleanupTest = onCleanup(@() evalc(removeCommand(testFile)));
%#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile)));
%#ok<NLS0004>

filewrite(testFile, {
  ['marker = ''', markerFile, ''';']
  'if ~isfile(marker)'
  '  filewrite(marker, ''failed once'');'
  '  asserts.istrue(false);'
  'end'
  'asserts.istrue(true);'
  });

suite = nelson.unittest.discover(testFile, 'Kind', 'test');
testPlan = nelson.unittest.plan(suite, 'Workers', 1);
retryPattern = [tempdir(), 'nelson_unittest_retry_*.json'];
retryCountBefore = length(dir(retryPattern));
results = nelson.unittest.run(testPlan, 'Retry', 1);
retryCountAfter = length(dir(retryPattern));
asserts.istrue(results.status);
asserts.isequal(results.summary.failed, 0);
asserts.isequal(length(results.cases), 1);
asserts.isequal(results.cases(1).outcome, 'passed');
asserts.isequal(results.cases(1).attempts, 2);
asserts.isequal(retryCountAfter, retryCountBefore);

%=============================================================================
