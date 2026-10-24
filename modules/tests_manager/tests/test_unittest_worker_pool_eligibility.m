%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [[tempdir(), ''], 'test_unittest_worker_pool_eligibility_', createGUID(), '.m'];
taggedFilename = [[tempdir(), ''], ...
  'test_unittest_worker_pool_eligibility_tagged_', createGUID(), '.m'];
taggedBenchFilename = [[tempdir(), ''], ...
  'bench_unittest_worker_pool_eligibility_tagged_', createGUID(), '.m'];
%#ok<NLS0004>
cleanupFile = onCleanup(@() evalc(['if isfile(''', filename, '''), rmfile(''', filename, '''), end']));
%#ok<NLS0004>
cleanupTaggedFile = onCleanup(@() evalc(['if isfile(''', taggedFilename, ...
  '''), rmfile(''', taggedFilename, '''), end']));
%#ok<NLS0004>
cleanupTaggedBenchFile = onCleanup(@() evalc(['if isfile(''', taggedBenchFilename, ...
  '''), rmfile(''', taggedBenchFilename, '''), end']));
filewrite(filename, {'clear all'; 'asserts.istrue(true);'});
filewrite(taggedFilename, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});
filewrite(taggedBenchFilename, {'% <--REUSE PROCESS-->'; 'tic(); 1 + 1; toc();'});

runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0);
testCase = initialize_test_case(filename, '', runtimeOptions);
asserts.isfalse(compute_worker_pool_eligibility(testCase));
taggedTestCase = initialize_test_case(taggedFilename, '', runtimeOptions);
asserts.istrue(compute_worker_pool_eligibility(taggedTestCase));
taggedBenchTestCase = initialize_test_case(taggedBenchFilename, '', runtimeOptions);
asserts.istrue(compute_worker_pool_eligibility(taggedBenchTestCase));

testCase.worker_pool_eligible = true;
asserts.istrue(can_pool_test_case(testCase));
testCase.worker_pool_eligible = false;
asserts.isfalse(can_pool_test_case(testCase));

annotated = annotate_worker_pool_eligibility(testCase);
asserts.isfalse(annotated.worker_pool_eligible);
taggedAnnotated = annotate_worker_pool_eligibility(taggedTestCase);
asserts.istrue(taggedAnnotated.worker_pool_eligible);
%=============================================================================
