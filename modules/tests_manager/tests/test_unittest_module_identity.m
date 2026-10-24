%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
moduleRoot = modulepath('tests_manager');
privateDir = fullfile(moduleRoot, 'functions', '+nelson', '+unittest', 'private');
savedPath = path();
savedDirectory = pwd();
addpath(fullfile(moduleRoot, 'functions'));
addpath(privateDir);
addpath(fullfile(moduleRoot, 'tests'));
testRoot = fullfile(tempdir(), ['module_identity_', createGUID()]);
mkdir(testRoot);
legacyName = ['legacy_identity_', strrep(createGUID(), '-', '_')];
cleanup = onCleanup(@() cleanupModuleIdentity(testRoot, savedPath, savedDirectory, legacyName));

coreFile = fullfile(nelsonroot(), 'modules', 'string', 'tests', 'test_strfind.m');
[name, root] = discover_moduleNameFromFile(coreFile);
asserts.isequal(name, 'string');
asserts.isequal(root, replace(fullpath(fullfile(nelsonroot(), 'modules', 'string')), '\', '/'));
if ispc()
  asserts.isequal(discover_moduleNameFromFile(upper(coreFile)), 'string');
end
optionalFile = fullfile(nelsonroot(), 'modules', 'graphics', 'tests', 'test_stem3.m');
asserts.isequal(discover_moduleNameFromFile(optionalFile), 'graphics');

relativeRoots = {'source-checkout', 'modules/declared/1.0.0', ...
  'modules/.nmm-tmp/012345', 'project/modules/misleading-directory'};
for k = 1:numel(relativeRoots)
  fixtureRoot = fullfile(testRoot, relativeRoots{k});
  file = createDiscoveryModule(fixtureRoot, 'declared_identity', false);
  [name, root] = discover_moduleNameFromFile(file);
  asserts.isequal(name, 'declared_identity');
  asserts.isequal(root, replace(fullpath(fixtureRoot), '\', '/'));
  suite = nelson.unittest.discover(file);
  asserts.isequal(suite.count, 1);
  asserts.isequal(suite.cases.module, 'declared_identity');
  nested = fullfile(fixtureRoot, 'tests', 'nested', 'deeper');
  mkdir(nested);
  nestedFile = fullfile(nested, 'test_nested.m');
  copyfile(file, nestedFile);
  [name, root] = discover_moduleNameFromFile(nestedFile);
  asserts.isequal(name, 'declared_identity');
  asserts.isequal(root, replace(fullpath(fixtureRoot), '\', '/'));
  if ispc()
    asserts.isequal(discover_moduleNameFromFile(replace(file, '/', '\')), 'declared_identity');
  end
end
cd(testRoot);
asserts.isequal(discover_moduleNameFromFile('source-checkout/tests/../tests/test_identity.m'), 'declared_identity');
cd(savedDirectory);

fakeRoot = fullfile(testRoot, 'fake', 'modules', 'misleading', 'tests');
mkdir(fakeRoot);
fakeFile = fullfile(fakeRoot, 'test_fake.m');
filewrite(fakeFile, 'asserts.istrue(true);');
[name, root] = discover_moduleNameFromFile(fakeFile);
asserts.isequal(name, '');
asserts.isequal(root, '');
asserts.isequal(discover_moduleNameFromFile(fullfile(fakeRoot, 'does_not_exist.m')), '');
asserts.isequal(discover_moduleNameFromFile(''), '');
% Even a plausible directory name cannot replace a manifest or registration.
mkdir(fullfile(fileparts(fakeRoot), 'etc'));
filewrite(fullfile(fileparts(fakeRoot), 'etc', 'startup.m'), '');
asserts.isequal(discover_moduleNameFromFile(fakeFile), '');

brokenRoot = fullfile(testRoot, 'broken');
file = createDiscoveryModule(brokenRoot, 'declared_identity', false);
invalidManifests = {'{', '{}', '[]', '{"module":42}', '{"module":".nmm-tmp"}', ...
  '{"module":"bad/name"}', '{"module":["first","second"]}'};
for k = 1:numel(invalidManifests)
  filewrite(fullfile(brokenRoot, 'module.json'), invalidManifests{k});
  [name, root] = discover_moduleNameFromFile(file);
  asserts.isequal(name, '');
  asserts.isequal(root, '');
end
filewrite(fullfile(brokenRoot, 'module.json'), '{"module":"updated_identity"}');
asserts.isequal(discover_moduleNameFromFile(file), 'updated_identity');
% Metadata without a loadable module boundary does not trigger an autoload.
metadataRoot = fullfile(testRoot, 'metadata_only');
mkdir(fullfile(metadataRoot, 'tests'));
filewrite(fullfile(metadataRoot, 'module.json'), '{"module":"not_loadable"}');
filewrite(fullfile(metadataRoot, 'tests', 'test_probe.m'), '');
asserts.isequal(discover_moduleNameFromFile(fullfile(metadataRoot, 'tests', 'test_probe.m')), '');

legacyRoot = fullfile(testRoot, 'unrelated_legacy_directory');
mkdir(fullfile(legacyRoot, 'etc'));
mkdir(fullfile(legacyRoot, 'tests'));
filewrite(fullfile(legacyRoot, 'etc', 'startup.m'), '');
filewrite(fullfile(legacyRoot, 'etc', 'finish.m'), '');
filewrite(fullfile(legacyRoot, 'tests', 'test_legacy.m'), '');
addmodule(legacyRoot, legacyName);
[name, root] = discover_moduleNameFromFile(fullfile(legacyRoot, 'tests', 'test_legacy.m'));
asserts.isequal(name, legacyName);
asserts.isequal(root, replace(fullpath(legacyRoot), '\', '/'));
filewrite(fullfile(legacyRoot, 'module.json'), '{"module":"manifest_identity"}');
asserts.isequal(discover_moduleNameFromFile(fullfile(legacyRoot, 'tests', 'test_legacy.m')), 'manifest_identity');
%=============================================================================
function cleanupModuleIdentity(testRoot, savedPath, savedDirectory, legacyName)
  if ismodule(legacyName)
    removemodule(legacyName);
  end
  cd(savedDirectory);
  path(savedPath);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
end
