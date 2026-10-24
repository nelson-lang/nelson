%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
nelson.unittest.skip(false, 'no skip');
skip_testsuite(false, 'no skip');
nelson.unittest.assume(true, 'assumption satisfied');

previousWorkers = maxNumCompThreads(1);
cleanupWorkers = onCleanup(@() maxNumCompThreads(previousWorkers)); %#ok<NLS0004>

testFile = [nelsonroot(), '/modules/tests_manager/tests/test_skip_testsuite_condition_false.m'];
output = evalc('status = test_run(testFile);');
asserts.istrue(status);
asserts.istrue(contains(output, 'Pass'));
results = nelson.unittest.run(testFile);
asserts.isequal(results.type, 'TestRunResult');
asserts.istrue(results.status);
%=============================================================================
