%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% <--WEIGHT 8-->
testRoot = [tempdir(), 'test_nmm/'];
externalModulesRoot = [testRoot, 'external_modules/'];
helpCacheRoot = [testRoot, 'help_cache/'];
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
previousHelpCacheDir = getenv('NELSON_HELP_CACHE_DIR');
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
mkdir(externalModulesRoot);
setenv('NELSON_EXTERNAL_MODULES_PATH', externalModulesRoot);
setenv('NELSON_HELP_CACHE_DIR', helpCacheRoot);
seedMinimalHelpCache(helpCacheRoot);
%=============================================================================
try
  r = nmm('list');
  asserts.isequal(class(r), 'struct');
  quietListText = evalc('quietList = nmm(''list'', ''-quiet'');');
  asserts.isequal(strtrim(quietListText), '');
  asserts.isequal(class(quietList), 'struct');
  commands = nmm('commands');
  asserts.istrue(commandContains(commands, 'install'));
  asserts.istrue(commandContains(commands, 'help'));
  asserts.istrue(commandAliasContains(commands, 'maintenance', 'orphans'));
  asserts.istrue(commandAliasContains(commands, 'maintenance', 'autoremove'));
  commandsJson = jsondecode(nmm('commands', '-json'));
  asserts.istrue(commandContains(commandsJson, 'install'));
  helpSummary = nmm('help');
  asserts.istrue(hasTextValue(helpSummary.commands, 'install'));
  helpSummaryJson = jsondecode(nmm('help', '-json'));
  asserts.istrue(hasTextValue(helpSummaryJson.commands, 'install'));
  installHelp = nmm('help', 'install');
  asserts.isequal(installHelp.command, 'install');
  installHelpJson = jsondecode(nmm('help', 'install', '-json'));
  asserts.isequal(installHelpJson.command, 'install');
  autoremoveHelp = nmm('help', 'autoremove');
  asserts.isequal(autoremoveHelp.command, 'maintenance');
  removeHelp = nmm('help', 'uninstall');
  asserts.isequal(removeHelp.command, 'remove');
  asserts.checkerror('nmm(''help'', ''missing_command'')', _('Unknown nmm command: missing_command'));
  asserts.checkerror('nmm(''missing_command'')', '#1 value or type not managed.', 'Nelson:nmm:UnknownCommand');
  archiveFile = [modulepath('modules_manager', 'tests'), '/module_skeleton_basic-all-3.0.0.nmz'];
  needToRestore = any(contains(fieldnames(r), 'module_skeleton_basic'));
  if isempty(needToRestore)
    needToRestore = false;
  end
  if needToRestore
    nmm('uninstall', 'module_skeleton_basic')
  end
  %=============================================================================
  nmm('install', archiveFile, '-force');
  r = nmm('list');
  r = any(contains(fieldnames(r), 'module_skeleton_basic'));
  asserts.istrue(r);
  asserts.istrue(ismodule('module_skeleton_basic'));
  %=============================================================================
  nmm('load', 'module_skeleton_basic');
  asserts.istrue(ismodule('module_skeleton_basic'));
  r = nmm('list');
  asserts.istrue(isstruct(r.module_skeleton_basic));
  asserts.istrue(isdir(r.module_skeleton_basic.path));
  asserts.istrue(r.module_skeleton_basic.load);
  listJson = jsondecode(nmm('list', '-json'));
  asserts.istrue(isfield(listJson, 'module_skeleton_basic'));
  %=============================================================================
  R = nmm('autoload', 'module_skeleton_basic');
  asserts.istrue(R);
  %=============================================================================
  nmm('autoload', 'module_skeleton_basic', true);
  r = nmm('list');
  asserts.istrue(r.module_skeleton_basic.load);
  R = nmm('autoload', 'module_skeleton_basic');
  asserts.istrue(R);
  %=============================================================================
  filename = nmm('package', 'module_skeleton_basic', tempdir());
  asserts.istrue(isfile(filename));
  asserts.istrue(isfile([filename, '.sha256']));
  asserts.isequal(strtrim(fileread([filename, '.sha256'])), sha256(filename, '-file'));
  %=============================================================================
  nmm('uninstall', 'module_skeleton_basic');
  r = nmm('list');
  r = contains(fieldnames(r), 'module_skeleton_basic');
  if isempty(r)
    r = false;
  end
  asserts.isfalse(r);
  %=============================================================================
  nmm('install', archiveFile, '-force');
  r = nmm('list');
  r = any(contains(fieldnames(r), 'module_skeleton_basic'));
  asserts.istrue(r);
  nmm('uninstall', 'module_skeleton_basic');
  r = nmm('list');
  r = contains(fieldnames(r), 'module_skeleton_basic');
  if isempty(r)
    r = false;
  end
  asserts.isfalse(r);
  %=============================================================================
  if needToRestore
    nmm('install', archiveFile);
  end
  restoreExternalModulesPath(previousExternalModulesPath);
  restoreHelpCacheDir(previousHelpCacheDir);
  rmdir(testRoot, 's');
catch e
  cleanupInstalledModule('module_skeleton_basic');
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
function tf = commandContains(commands, command)
  tf = false;
  for k = 1:length(commands)
    if strcmp(commands(k).command, command)
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = commandAliasContains(commands, command, alias)
  tf = false;
  for k = 1:length(commands)
    if strcmp(commands(k).command, command) && any(strcmp(commands(k).aliases, alias))
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = hasTextValue(values, expected)
  tf = false;
  if iscell(values)
    for k = 1:length(values)
      if ischar(values{k}) && strcmp(values{k}, expected)
        tf = true;
        return
      end
    end
  end
end
%=============================================================================
