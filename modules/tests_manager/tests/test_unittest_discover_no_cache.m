%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
base = [[tempdir(), ''], 'test_unittest_discover_no_cache_', createGUID()];
testFile = [base, '.m'];
cleanupFile = onCleanup(@() run_removeFile(testFile)); %#ok<NLS0004>

filewrite(testFile, {'asserts.istrue(true);'});
first = nelson.unittest.discover(testFile, 'Kind', 'test');
asserts.isequal(first.count, 1);
asserts.isfalse(any(strcmp(first.cases(1).tags, 'reuse_process')));
asserts.isequal(first.cases(1).timeout, 140);

filewrite(testFile, {'% <--REUSE PROCESS-->'; '% <--WEIGHT 4-->'; ...
  '% <--TIMEOUT 250-->'; 'asserts.istrue(true);'});
second = nelson.unittest.discover(testFile, 'Kind', 'test');
asserts.istrue(any(strcmp(second.cases(1).tags, 'reuse_process')));
asserts.isequal(second.cases(1).weight, 4);
asserts.isequal(second.cases(1).timeout, 250);
asserts.isfalse(any(strcmp(second.cases(1).tags, 'timeout')));
run_removeFile(testFile);
%=============================================================================
