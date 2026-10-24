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
moduleName = 'nmm_portable_probe';
sourceRoot = [tempdir(), moduleName, '/'];
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousRegistry = getenv('NELSON_NMM_REGISTRY');
externalModulesRoot = [tempdir(), 'nmm_portable_modules/'];
if isdir(sourceRoot), rmdir(sourceRoot, 's'); end
if isdir(externalModulesRoot), rmdir(externalModulesRoot, 's'); end
mkdir(sourceRoot);
mkdir([sourceRoot, 'etc']);
mkdir([sourceRoot, 'functions']);
mkdir([sourceRoot, 'help/en_US/xml']);
mkdir([sourceRoot, 'tests']);
mkdir(externalModulesRoot);
setenv('NELSON_EXTERNAL_MODULES_PATH', externalModulesRoot);
moduleJson = [
  "{";
  ['  "module": "', moduleName, '",'];
  ['  "title": "', moduleName, '",'];
  "  ""summary"": ""Portable local package installation probe"",";
  "  ""version"": ""1.0.0"",";
  "  ""platforms"": [""all""],";
  "  ""nelson"": "">=1.0.0"",";
  "  ""builtin"": false,";
  "  ""license"": ""BSD-3-Clause"",";
  "  ""dependencies"": {}";
  "}"
];
filewrite([sourceRoot, 'module.json'], moduleJson);
filewrite([sourceRoot, 'builder.m'], [
  "moduleRoot = fileparts(nfilename('fullpath'));";
  ['nmm_build_loader(''', moduleName, ''', moduleRoot);']
]);
filewrite([sourceRoot, 'etc/startup.m'], ['addpath([modulepath(''', moduleName, '''), ''/functions'']);']);
filewrite([sourceRoot, 'etc/finish.m'], ['rmpath([modulepath(''', moduleName, '''), ''/functions'']);']);
filewrite([sourceRoot, 'functions/nmm_portable_value.m'], [
  "function value = nmm_portable_value()";
  "  value = 42;";
  "end"
]);
filewrite([sourceRoot, 'help/en_US/xml/chapter.xml'], '<chapter></chapter>');
filewrite([sourceRoot, 'tests/test_placeholder.m'], '');
try
  registryFile = [sourceRoot, 'registry.json'];
  filewrite(registryFile, '{"packages":[]}');
  filewrite([registryFile, '.sha256'], [sha256(registryFile, '-file'), '  registry.json']);
  setenv('NELSON_NMM_REGISTRY', registryFile);
  asserts.istrue(isempty(nmm('registry', 'search', '')));
  nmm('install', sourceRoot);
  installed = nmm('list');
  asserts.istrue(isfield(installed, moduleName));
  asserts.isequal(nmm_portable_value(), 42);
  nmm('pin', moduleName, '1.0.0');
  nmm('unpin', moduleName);
  nmm('uninstall', moduleName);
  asserts.isfalse(isfield(nmm('list'), moduleName));
  setenv('NELSON_NMM_REGISTRY', previousRegistry);
  setenv('NELSON_EXTERNAL_MODULES_PATH', previousExternalModulesPath);
  rmdir(sourceRoot, 's');
  rmdir(externalModulesRoot, 's');
catch e
  try, nmm('uninstall', moduleName); catch, end
  setenv('NELSON_NMM_REGISTRY', previousRegistry);
  setenv('NELSON_EXTERNAL_MODULES_PATH', previousExternalModulesPath);
  if isdir(sourceRoot), rmdir(sourceRoot, 's'); end
  if isdir(externalModulesRoot), rmdir(externalModulesRoot, 's'); end
  rethrow(e);
end
