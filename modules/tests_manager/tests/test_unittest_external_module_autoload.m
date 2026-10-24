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
moduleRoot = fileparts(fileparts(nfilename('fullpath')));
repoRoot = fileparts(fileparts(moduleRoot));
savedPath = path();
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
addpath(fullfile(moduleRoot, 'functions'));
addpath(fullfile(moduleRoot, 'tests'));
testRoot = fullfile(repoRoot, 'temp', ['external_autoload_', createGUID()]);
mkdir(testRoot);
cleanup = onCleanup(@() cleanupExternalIdentity(testRoot, savedPath, previousForceNativeRunner));
relativeRoots = {'source', 'modules/some-directory/1.0.0', 'modules/.nmm-tmp/staged'};
files = {};
names = {};
for k = 1:numel(relativeRoots)
  names{k} = ['identity_case_', num2str(k), '_', strrep(createGUID(), '-', '_')];
  root = fullfile(testRoot, relativeRoots{k});
  files{k} = createDiscoveryModule(root, names{k}, false);
end
% A nested test must load the package root, not the tests subdirectory.
nested = fullfile(fileparts(files{3}), 'nested');
mkdir(nested);
copyfile(files{3}, fullfile(nested, 'test_nested_identity.m'));
files{4} = fullfile(nested, 'test_nested_identity.m');
names{4} = names{3};
results = nelson.unittest.run(files, 'Workers', 2);
asserts.istrue(results.status);
asserts.isequal(results.summary.passed, 4);
for k = 1:numel(results.cases)
  asserts.istrue(any(strcmp(results.cases(k).module, names)));
  asserts.isequal(results.cases(k).outcome, 'passed');
end
% The compatibility entry point uses the same resolver and fresh worker setup.
asserts.istrue(test_run(files{3}, '-stoponfail'));
asserts.isfalse(ismodule('.nmm-tmp'));
%=============================================================================
function cleanupExternalIdentity(testRoot, savedPath, previousForceNativeRunner)
  setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner);
  path(savedPath);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
end
