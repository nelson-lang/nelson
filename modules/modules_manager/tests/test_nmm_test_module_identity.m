%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
% <--WEIGHT 4-->
skip_testsuite(isempty(which('__nelson_unittest_process_supervisor_start__')), ...
  'Native unittest process supervisor is not available.');
savedPath = path();
previousExternal = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
addpath(fullfile(modulepath('tests_manager'), 'functions'));
addpath(fullfile(modulepath('tests_manager'), 'tests'));
testRoot = fullfile(tempdir(), ['nmm_test_identity_', createGUID()]);
mkdir(testRoot);
mkdir(fullfile(testRoot, 'modules'));
mkdir(fullfile(testRoot, 'archives'));
setenv('NELSON_EXTERNAL_MODULES_PATH', fullfile(testRoot, 'modules'));
cleanup = onCleanup(@() cleanupNmmIdentity(testRoot, savedPath, previousExternal, previousForceNativeRunner));
name = ['nmm_identity_', strrep(createGUID(), '-', '_')];
source = fullfile(testRoot, 'source_checkout');
createDiscoveryModule(source, name, true);
archive = nmm('pack', source, fullfile(testRoot, 'archives'));
asserts.istrue(isfile(archive));
asserts.istrue(isfile([archive, '.sha256']));
unpacked = fullfile(testRoot, 'unpacked');
mkdir(unpacked);
unzip(archive, unpacked);
manifest = jsondecode(fullfile(unpacked, 'module.json'), '-file');
asserts.isequal(manifest.module, name);
suite = nelson.unittest.discover(fullfile(unpacked, 'tests', 'test_identity.m'));
asserts.isequal(suite.cases.module, name);
asserts.istrue(test_run(fullfile(unpacked, 'tests', 'test_identity.m'), '-stoponfail'));
asserts.isfalse(ismodule('.nmm-tmp'));
%=============================================================================
function cleanupNmmIdentity(testRoot, savedPath, previousExternal, previousForceNativeRunner)
  setenv('NELSON_EXTERNAL_MODULES_PATH', previousExternal);
  setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner);
  path(savedPath);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
end
