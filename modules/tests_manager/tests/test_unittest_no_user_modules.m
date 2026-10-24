%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
badModulesRoot = [[tempdir(), ''], 'test_unittest_bad_modules_', createGUID(), '/'];
testFile = [[tempdir(), ''], 'test_unittest_no_user_modules_', createGUID(), '.m'];
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');

previousExternalModulesPathLiteral = ['''', strrep(previousExternalModulesPath, '''', ''''''), ''''];
testFileLiteral = ['''', strrep(testFile, '''', ''''''), ''''];
badModulesRootLiteral = ['''', strrep(badModulesRoot, '''', ''''''), ''''];
cleanupCommand = ['setenv(''NELSON_EXTERNAL_MODULES_PATH'', ', previousExternalModulesPathLiteral, ');', newline, ...
  'if isfile(', testFileLiteral, '), rmfile(', testFileLiteral, '), end', newline, ...
  'if isdir(', badModulesRootLiteral, '), rmdir(', badModulesRootLiteral, ', ''s''), end'];
cleanupTest = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

mkdir(badModulesRoot);
fid = fopen([badModulesRoot, 'modules.json'], 'wb');
fwrite(fid, uint8([255 254 0 123]));
fclose(fid);
setenv('NELSON_EXTERNAL_MODULES_PATH', badModulesRoot);

filewrite(testFile, {'asserts.istrue(true);'});
results = nelson.unittest.run(testFile, 'Workers', 1);
asserts.istrue(results.status);
asserts.isequal(results.cases(1).outcome, 'passed');
asserts.istrue(contains(results.cases(1).diagnostics.process_arguments, '--no-user-modules'));
%=============================================================================
