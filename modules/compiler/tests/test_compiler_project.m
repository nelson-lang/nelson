%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 180-->
ncc('--help');
check_project();

function check_project()
  work = [tempname(), ' project '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  filename = fullfile(work, 'app.ncproj');
  project = __nelsonc_project__('new');
  asserts.isequal(project.Version, 6);
  options = compiler.package.InstallerOptions('ApplicationName', 'Application');
  asserts.isequal(sort(fieldnames(project.Installer)), sort(properties(options)));
  asserts.isequal(project.Target, 'standaloneApplication');
  __nelsonc_project__('save', project, filename);
  asserts.isequal(__nelsonc_project__('load', filename), project);
  before = sha256(filename, '-file');
  rejected(@() __nelsonc_project__('save', project, filename));
  asserts.isequal(sha256(filename, '-file'), before);
  invalid = project;
  invalid.Version = 999;
  rejected(@() __nelsonc_project__('save', invalid, filename, true));
  asserts.isequal(sha256(filename, '-file'), before);
  invalid = project;
  invalid.Command = 'error(''not project data'')';
  rejected(@() __nelsonc_project__('validate', invalid));
  invalid = project;
  invalid.Build.UnknownOption = true;
  rejected(@() __nelsonc_project__('validate', invalid));
  invalid = project;
  invalid.Installer.UnknownOption = '';
  rejected(@() __nelsonc_project__('validate', invalid));
  invalid = project;
  invalid.Installer = rmfield(invalid.Installer, 'InstallerLogo');
  rejected(@() __nelsonc_project__('validate', invalid));
  invalid = project;
  invalid.Installer.RuntimeDelivery = 'web';
  rejected(@() __nelsonc_project__('validate', invalid));
  broken = fullfile(work, 'broken.ncproj');
  filewrite(broken, '{"incomplete":');
  rejected(@() __nelsonc_project__('load', broken));
  rejected(@() __nelsonc_project__('save', project, fullfile(work, 'invalid.m')));
  entry = fullfile(work, 'project_entry.m');
  filewrite(entry, 'function project_entry(); disp(''PROJECT_ENTRY_OK''); end');
  resource = fullfile(work, 'data.txt');
  filewrite(resource, 'kept resource');
  project.Build.AppFile = entry;
  project.Build.OutputDir = fullfile(work, 'build');
  project.Build.AdditionalFiles = {resource};
  __nelsonc_project__('save', project, filename, true);
  restored = __nelsonc_project__('load', filename);
  expected = __nelsonc_project__('validate', project);
  asserts.isequal(restored, expected);
  plan = __nelsonc_project__('analyze', restored);
  asserts.istrue(plan.complete);
  asserts.isfalse(any(ismember(plan.runtime.modules, {'compiler', 'graphics', 'image_processing'})));
  asserts.isfalse(isfolder(project.Build.OutputDir));
  command = __nelsonc_project__('command', restored);
  eval(command);
  asserts.istrue(isa(results, 'compiler.build.Results'));
  digest = sha256(results.Files{1}, '-file');
  project.Build.OutputDir = fullfile(work, 'repeat');
  result = __nelsonc_project__('build', project);
  asserts.isequal(sha256(result.Files{1}, '-file'), digest);
  project.Installer.OutputDir = fullfile(work, 'installer');
  settings = __nelsonc_project__('installer-options', project, result);
  asserts.isequal(settings.RuntimeDelivery, 'none');
  asserts.isequal(settings.Shortcut, '');
  asserts.isequal(settings.ApplicationName, 'project_entry');
  if ispc()
    __nelsonc_project__('package', project, result);
    asserts.istrue(isfile(fullfile(settings.OutputDir, 'MyAppInstaller.exe')));
  end
  rmfile(entry);
  rmfile(resource);
  restored = __nelsonc_project__('load', filename);
  asserts.isequal(restored.Build.AppFile, expected.Build.AppFile);
  rejected(@() __nelsonc_project__('analyze', restored));
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  [status, text] = system(['"', result.Files{1}, '"'], 60);
  asserts.isequal(status, 0, text);
  asserts.istrue(contains(text, 'PROJECT_ENTRY_OK'));
  clear restore;
  clear cleanup;
end

function rejected(callback)
  failed = false;
  try; callback(); catch ex; failed = ~isempty(ex.message); end
  asserts.istrue(failed);
end
