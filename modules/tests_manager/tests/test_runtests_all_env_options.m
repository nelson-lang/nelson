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
previousShardIndex = getenv('NELSON_TEST_SHARD_INDEX');
previousShardCount = getenv('NELSON_TEST_SHARD_COUNT');
previousWorkers = getenv('NELSON_TEST_WORKERS');
previousValidate = getenv('NELSON_TESTS_ALL_VALIDATE_OPTIONS_ONLY');
cleanupShardIndex = onCleanup(@() setenv('NELSON_TEST_SHARD_INDEX', previousShardIndex));
cleanupShardCount = onCleanup(@() setenv('NELSON_TEST_SHARD_COUNT', previousShardCount));
cleanupWorkers = onCleanup(@() setenv('NELSON_TEST_WORKERS', previousWorkers));
cleanupValidate = onCleanup(@() setenv('NELSON_TESTS_ALL_VALIDATE_OPTIONS_ONLY', previousValidate));

setenv('NELSON_TEST_SHARD_INDEX', '2');
setenv('NELSON_TEST_SHARD_COUNT', '3');
setenv('NELSON_TEST_WORKERS', '1');
setenv('NELSON_TESTS_ALL_VALIDATE_OPTIONS_ONLY', 'TRUE');

binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-cli'];
if ispc()
  nelson_exe = [nelson_exe, '.exe'];
end
skip_testsuite(~isfile(nelson_exe), ['Executable not found: ', nelson_exe]);

script = [nelsonroot(), '/tools/tests_all/runtests_all.m'];
skip_testsuite(~isfile(script), ['Test runner not found: ', script]);
cmd = ['"', nelson_exe, '" --quiet --no-ipc --no-user-modules -f "', script, '"'];
[status, output] = system(cmd, 30);

asserts.isequal(status, 0);
asserts.istrue(contains(output, 'NELSON_TEST_SHARD_INDEX=2'));
asserts.istrue(contains(output, 'NELSON_TEST_SHARD_COUNT=3'));
asserts.istrue(contains(output, 'NELSON_TEST_WORKERS=1'));
%=============================================================================
