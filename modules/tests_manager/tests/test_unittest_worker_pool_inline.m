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
baseFile = [nelsonroot(), '/modules/tests_manager/tests/test_unittest_worker_pool_inline.m'];
baseRuntimeOptions = struct('Timeout', [], 'LauncherGrace', 0, ...
  'WorkerPoolInline', false);
base = initialize_test_case(baseFile, '', baseRuntimeOptions);

defaultOptions = run_parseRunOptions({});
asserts.isfalse(defaultOptions.WorkerPoolInline);

runtimeOptions = struct('WorkerPoolInline', true);
privateDir = [modulepath('tests_manager'), '/functions/+nelson/+unittest/private/'];
worker = worker_pool_create_worker(base, privateDir, runtimeOptions);
asserts.isequal(worker.command_filename, '');
asserts.istrue(contains(worker.process_arguments, '--execute'));
asserts.istrue(contains(worker.process_arguments, ' -- --no-audio --without-python --without-julia'));

cmdWithUserArguments = append_process_user_arguments('nelson-cli --quiet', {'a b', '--inline'});
asserts.istrue(contains(cmdWithUserArguments, ' -- "a b" "--inline"'));

runtimeOptions.WorkerPoolInline = false;
worker = worker_pool_create_worker(base, privateDir, runtimeOptions);
asserts.isfalse(isempty(worker.command_filename));
asserts.istrue(contains(worker.process_arguments, '--file'));
asserts.istrue(contains(worker.process_arguments, ' -- --no-audio --without-python --without-julia'));
run_removeFile(worker.command_filename);

staleTestDir = [[tempdir(), ''], 'test_unittest_worker_pool_stale_', createGUID(), '/'];
mkdir(staleTestDir);
staleWorkerFile = [staleTestDir, 'nelson_unittest_worker_', createGUID(), '.m'];
otherFile = [staleTestDir, 'nelson_unittest_not_worker_', createGUID(), '.m'];
cleanupCommand = ['if isdir(''', staleTestDir, '''), ', ...
  'rmdir(''', staleTestDir, ''', ''s''), end'];
%#ok<NLS0004>
cleanupStaleFiles = onCleanup(@() evalc(cleanupCommand));
filewrite(staleWorkerFile, {'disp(''stale'');'});
filewrite(otherFile, {'disp(''keep'');'});
worker_pool_cleanup_stale_scripts(-1, staleTestDir);
asserts.isfalse(isfile(staleWorkerFile));
asserts.istrue(isfile(otherFile));

longCode = repmat('x', 1, worker_pool_inline_limit() + 1);
asserts.isfalse(worker_pool_can_inline(longCode, ''));
if isdir(staleTestDir)
  rmdir(staleTestDir, 's');
end
%=============================================================================
