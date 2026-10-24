%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
resultFile = [[tempdir(), ''], 'test_unittest_junit_xml_', createGUID(), '.xml'];
cleanup = onCleanup(@() evalc(removeCommand(resultFile))); %#ok<NLS0004>

testFile = [nelsonroot(), '/modules/tests_manager/tests/test_not_fixed.m'];
output = evalc('status = test_run(testFile, resultFile);');
asserts.istrue(status);
asserts.istrue(contains(output, 'test_not_fixed'));
asserts.istrue(isfile(resultFile));

xml = fileread(resultFile);
asserts.istrue(contains(xml, '<skipped/>'));
asserts.istrue(contains(xml, '</testcase>'));
asserts.istrue(strfind(xml, '<skipped/>') < strfind(xml, '</testcase>'));
%=============================================================================
