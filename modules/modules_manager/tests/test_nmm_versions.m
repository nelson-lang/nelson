%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% <--WEIGHT 16-->
testRoot = [tempdir(), 'nmm_versions/'];
externalModulesRoot = [testRoot, 'external_modules/'];
helpCacheRoot = [testRoot, 'help_cache/'];
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousHelpCacheDir = getenv('NELSON_HELP_CACHE_DIR');
moduleName = 'nmm_versions_module';
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
mkdir(externalModulesRoot);
setenv('NELSON_EXTERNAL_MODULES_PATH', externalModulesRoot);
setenv('NELSON_HELP_CACHE_DIR', helpCacheRoot);
seedMinimalHelpCache(helpCacheRoot);
cleanupInstalledModule(moduleName);
%=============================================================================
try
  v1Root = [testRoot, moduleName, '_1/'];
  v2Root = [testRoot, moduleName, '_2/'];
  v3Root = [testRoot, moduleName, '_3/'];
  createLocalModule(v1Root, moduleName, '1.0.0', 1);
  createLocalModule(v2Root, moduleName, '2.0.0', 2);
  createLocalModule(v3Root, moduleName, '3.0.0', 3);

  nmm('install', v1Root, '-no-tests');
  asserts.isequal(nmm_versions_module_value(), 1);
  writeLegacyModulesJson(moduleName);

  nmm('install', v2Root, '-no-tests');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(moduleData.version, '2.0.0');
  asserts.isequal(length(moduleData.versions), 2);
  asserts.isequal(nmm_versions_module_value(), 2);
  installed = nmm('installed', moduleName);
  asserts.isequal(length(installed), 2);
  asserts.istrue(hasTextValue(installed, '1.0.0'));
  asserts.istrue(hasTextValue(installed, '2.0.0'));

  pinActiveReport = nmm('pin', moduleName);
  asserts.isequal(pinActiveReport.version, '2.0.0');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(moduleData.pinned_version, '2.0.0');
  asserts.isequal(nmm_versions_module_value(), 2);

  pinReport = nmm('pin', moduleName, '1.0.0');
  asserts.isequal(pinReport.version, '1.0.0');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(moduleData.version, '1.0.0');
  asserts.isequal(moduleData.pinned_version, '1.0.0');
  asserts.isequal(nmm_versions_module_value(), 1);

  if ismodule(moduleName)
    removemodule(moduleName);
  end
  nmm('install', v3Root, '-no-tests');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(moduleData.version, '1.0.0');
  asserts.isequal(moduleData.pinned_version, '1.0.0');
  asserts.isequal(length(moduleData.versions), 3);
  asserts.isequal(nmm_versions_module_value(), 1);

  unpinReport = nmm('unpin', moduleName);
  asserts.istrue(unpinReport.unpinned);
  asserts.isequal(unpinReport.version, '3.0.0');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(moduleData.version, '3.0.0');
  asserts.isfalse(isfield(moduleData, 'pinned_version'));
  asserts.isequal(nmm_versions_module_value(), 3);

  nmm('load', moduleName, '2.0.0');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(moduleData.version, '2.0.0');
  asserts.isequal(nmm_versions_module_value(), 2);

  nmm('remove', moduleName, '2.0.0');
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  asserts.isequal(length(moduleData.versions), 2);
  asserts.isfalse(hasVersion(moduleData.versions, '2.0.0'));

  nmm('remove', moduleName, '1.0.0');
  nmm('remove', moduleName, '3.0.0');
  asserts.isfalse(isfield(nmm('list'), moduleName));
  asserts.isequal(length(nmm('installed', moduleName)), 0);

  cleanupInstalledModule(moduleName);
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  rmdir(testRoot, 's');
catch e
  cleanupInstalledModule(moduleName);
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  if isdir(testRoot)
    rmdir(testRoot, 's');
  end
  rethrow(e);
end
%=============================================================================
function restoreExternalModulesPath(value)
  setenv('NELSON_EXTERNAL_MODULES_PATH', value);
end
%=============================================================================
function restoreHelpCacheDir(value)
  setenv('NELSON_HELP_CACHE_DIR', value);
end
%=============================================================================
function seedMinimalHelpCache(root)
  for lang = getavailablelanguages()'
    langDir = [root, lang{1}, '/'];
    if ~isdir(langDir)
      mkdir(langDir);
    end
    filewrite([langDir, 'index.html'], '<!doctype html><html><body></body></html>');
    filewrite([langDir, 'index.json'], '{}');
  end
end
%=============================================================================
function createLocalModule(moduleRoot, moduleName, moduleVersion, value)
  mkdir(moduleRoot);
  mkdir([moduleRoot, 'etc']);
  mkdir([moduleRoot, 'functions']);
  mkdir([moduleRoot, 'help']);
  mkdir([moduleRoot, 'help/en_US']);
  mkdir([moduleRoot, 'help/en_US/xml']);
  mkdir([moduleRoot, 'tests']);
  moduleJson = [
    "{";
    ['  "module": "', moduleName, '",'];
    ['  "title": "', moduleName, '",'];
    ['  "summary": "nmm versions fixture",'];
    ['  "version": "', moduleVersion, '",'];
    ['  "platforms": ["all"],'];
    ['  "nelson": ">=1.0.0",'];
    ['  "builtin": false,'];
    ['  "license": "BSD-3-Clause",'];
    ['  "dependencies": {}'];
    "}"
  ];
  filewrite([moduleRoot, 'module.json'], moduleJson);
  builder = [
    "moduleRoot = fileparts(nfilename('fullpath'));";
    ['nmm_build_loader(''', moduleName, ''', moduleRoot);']
  ];
  filewrite([moduleRoot, 'builder.m'], builder);
  startup = ['addpath([modulepath(''', moduleName, '''), ''/functions'']);'];
  finish = ['rmpath([modulepath(''', moduleName, '''), ''/functions'']);'];
  filewrite([moduleRoot, 'etc/startup.m'], startup);
  filewrite([moduleRoot, 'etc/finish.m'], finish);
  body = ['function r = ', moduleName, '_value()', newline, '  r = ', int2str(value), ';', newline, 'end'];
  filewrite([moduleRoot, 'functions/', moduleName, '_value.m'], body);
  filewrite([moduleRoot, 'help/en_US/xml/chapter.xml'], '<chapter></chapter>');
  filewrite([moduleRoot, 'tests/test_placeholder.m'], '');
end
%=============================================================================
function cleanupInstalledModule(moduleName)
  try
    modules = nmm('list');
    if isfield(modules, moduleName)
      nmm('uninstall', moduleName);
    end
  catch
  end
end
%=============================================================================
function writeLegacyModulesJson(moduleName)
  modules = nmm('list');
  moduleData = getfield(modules, moduleName);
  if isfield(moduleData, 'versions')
    moduleData = rmfield(moduleData, 'versions');
  end
  if isfield(moduleData, 'pinned_version')
    moduleData = rmfield(moduleData, 'pinned_version');
  end
  modules.(moduleName) = moduleData;
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
end
%=============================================================================
function r = hasVersion(versions, version)
  r = false;
  for k = 1:length(versions)
    if strcmp(versions(k).version, version)
      r = true;
      return
    end
  end
end
%=============================================================================
function tf = hasTextValue(values, expected)
  tf = false;
  for k = 1:length(values)
    if ischar(values{k}) && strcmp(values{k}, expected)
      tf = true;
      return
    end
  end
end
%=============================================================================
