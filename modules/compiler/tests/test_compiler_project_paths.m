%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
check_project_paths();

function check_project_paths()
  previous = pwd();
  work = [tempname(), ' paths '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() cleanup_work(work, previous));
  root = fullfile(work, 'project');
  elsewhere = fullfile(work, 'elsewhere');
  mkdir(root);
  mkdir(elsewhere);
  entry = fullfile(root, 'entry.m');
  filewrite(entry, 'function entry(); end');
  helpFile = fullfile(root, 'help.txt');
  filewrite(helpFile, 'PROJECT_HELP');
  external = fullfile(work, 'project-other', 'external.txt');
  project = __nelsonc_project__('new');
  project.Build.AppFile = entry;
  project.Build.CustomHelpTextFile = helpFile;
  project.Build.OutputDir = fullfile(root, 'output');
  project.Build.AdditionalFiles = {fullfile(root, '*.nh5'), external, fullfile(root, '.hidden')};
  project.Build.RuntimeLogFile = 'logs/output.txt';
  project.Installer.OutputDir = fullfile(root, 'packages');
  project.Installer.Shortcut = fullfile(root, 'output', 'entry.exe');
  project.Installer.AdditionalFiles = {fullfile(root, 'license.txt')};
  project.Installer.DefaultInstallationDir = strrep(fullfile(root, 'on-target'), '\', '/');
  imageKeys = {'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'};
  if ispc()
    image = fullfile(root, 'brand.png');
    copyfile(fullfile(modulepath('compiler'), 'examples', 'standalone', 'app_icon.png'), image);
    project.Build.ExecutableIcon = image;
    for k = 1:numel(imageKeys); project.Installer.(imageKeys{k}) = image; end
  end
  project = __nelsonc_project__('validate', project);
  filename = fullfile(root, 'application.ncproj');
  __nelsonc_project__('save', project, filename);
  stored = jsondecode(fileread(filename));
  asserts.isequal(stored.Version, 6);
  asserts.isequal(stored.Build.AppFile, './entry.m');
  asserts.isequal(stored.Build.CustomHelpTextFile, './help.txt');
  asserts.isequal(stored.Build.OutputDir, './output');
  asserts.isequal(stored.Build.AdditionalFiles{1}, './*.nh5');
  asserts.isequal(stored.Build.AdditionalFiles{2}, strrep(external, '\', '/'));
  asserts.isequal(stored.Build.AdditionalFiles{3}, './.hidden');
  asserts.isequal(stored.Installer.OutputDir, './packages');
  asserts.isequal(stored.Installer.Shortcut, './output/entry.exe');
  asserts.isequal(stored.Installer.AdditionalFiles{1}, './license.txt');
  asserts.isequal(stored.Build.RuntimeLogFile, project.Build.RuntimeLogFile);
  asserts.isequal(stored.Installer.DefaultInstallationDir, project.Installer.DefaultInstallationDir);
  if ispc()
    asserts.isequal(stored.Build.ExecutableIcon, './brand.png');
    for k = 1:numel(imageKeys); asserts.isequal(stored.Installer.(imageKeys{k}), './brand.png'); end
  end
  cd(elsewhere);
  elsewhere = pwd();
  beforePath = path();
  asserts.isequal(__nelsonc_project__('load', filename), project);
  asserts.isequal(pwd(), elsewhere);
  asserts.isequal(path(), beforePath);
  legacy = fullfile(root, 'absolute.ncproj');
  old = rmfield(project, 'LastBuild');
  old.Version = 1;
  old.Build = rmfield(old.Build, {'TreatInputsAsNumeric', 'SupportPackages'});
  filewrite(legacy, jsonencode(old));
  asserts.isequal(__nelsonc_project__('load', legacy), project);
  old = rmfield(stored, 'LastBuild');
  old.Version = 2;
  old.Build = rmfield(old.Build, {'TreatInputsAsNumeric', 'SupportPackages'});
  filewrite(legacy, jsonencode(old));
  asserts.isequal(__nelsonc_project__('load', legacy), project);
  migrated = fullfile(root, 'migrated.ncproj');
  __nelsonc_project__('save', __nelsonc_project__('load', legacy), migrated);
  asserts.isequal(fileread(migrated), fileread(filename));
  old = stored;
  old.Version = 3;
  old.Build = rmfield(old.Build, {'TreatInputsAsNumeric', 'SupportPackages'});
  old.LastBuild = rmfield(old.LastBuild, 'SourcesSHA256');
  filewrite(legacy, jsonencode(old));
  asserts.isequal(__nelsonc_project__('load', legacy), project);
  savedDigest = sha256(filename, '-file');
  saveAs = fullfile(elsewhere, 'copy.ncproj');
  __nelsonc_project__('save', project, saveAs);
  asserts.isequal(sha256(filename, '-file'), savedDigest);
  asserts.isequal(__nelsonc_project__('load', saveAs), project);
  copy = jsondecode(fileread(saveAs));
  asserts.isequal(copy.Build.AppFile, project.Build.AppFile);
  check_relative_inputs(stored, root, elsewhere);
  check_invalid_inputs(stored, root, elsewhere);
  clear cleanup;
end

function check_relative_inputs(stored, root, elsewhere)
  filename = fullfile(root, 'relative.ncproj');
  stored.Build.AppFile = './nested/../entry.m';
  stored.Build.AdditionalFiles = {'../external.txt', './missing/link', './data?.mat'};
  stored.Installer.OutputDir = '.';
  filewrite(filename, jsonencode(stored));
  loaded = __nelsonc_project__('load', filename);
  asserts.isequal(loaded.Build.AppFile, strrep(fullfile(root, 'entry.m'), '\', '/'));
  asserts.isequal(loaded.Build.AdditionalFiles{1}, strrep(fullfile(fileparts(root), 'external.txt'), '\', '/'));
  asserts.isequal(loaded.Build.AdditionalFiles{2}, strrep(fullfile(root, 'missing', 'link'), '\', '/'));
  asserts.isequal(loaded.Build.AdditionalFiles{3}, strrep(fullfile(root, 'data?.mat'), '\', '/'));
  asserts.isequal(loaded.Installer.OutputDir, strrep(root, '\', '/'));
  asserts.isequal(pwd(), elsewhere);
  if ispc()
    loaded.Build.AppFile = [upper(fileparts(loaded.Build.AppFile)), '/ENTRY.m'];
    __nelsonc_project__('save', loaded, filename, true);
    saved = jsondecode(fileread(filename));
    asserts.isequal(saved.Build.AppFile, './ENTRY.m');
    loaded.Build.AdditionalFiles = {'Z:/external.txt', '//server/share/data.txt'};
    __nelsonc_project__('save', loaded, filename, true);
    saved = jsondecode(fileread(filename));
    asserts.isequal(saved.Build.AdditionalFiles(:), loaded.Build.AdditionalFiles(:));
    normalized = strrep(root, '\', '/');
    stored.Build.AppFile = [normalized(3:end), '/entry.m'];
    filewrite(filename, jsonencode(stored));
    loaded = __nelsonc_project__('load', filename);
    asserts.isequal(loaded.Build.AppFile, [normalized, '/entry.m']);
  end
end

function check_invalid_inputs(stored, root, elsewhere)
  filename = fullfile(root, 'invalid.ncproj');
  for invalid = {42, {'entry.m', 'second.m'}, 'Q:relative.m'}
    if ~ispc() && isequal(invalid{1}, 'Q:relative.m'); continue; end
    current = stored;
    current.Build.AppFile = invalid{1};
    filewrite(filename, jsonencode(current));
    rejected(@() __nelsonc_project__('load', filename), jsonencode(invalid{1}));
    asserts.isequal(pwd(), elsewhere);
  end
  current = stored;
  current.Build.AdditionalFiles = {'a', 'b'; 'c', 'd'};
  rejected(@() __nelsonc_project__('validate', current), 'AdditionalFiles must be a vector.');
  current.Build.AdditionalFiles = {42, 'invalid'};
  filewrite(filename, jsonencode(current));
  rejected(@() __nelsonc_project__('load', filename), 'AdditionalFiles must contain paths.');
  filewrite(filename, strrep(jsonencode(stored), './entry.m', 'entry.m\u0000'));
  rejected(@() __nelsonc_project__('load', filename), 'Paths cannot contain null characters.');
end

function rejected(callback, message)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed, message);
end

function cleanup_work(work, previous)
  cd(previous);
  if isfolder(work); rmdir(work, 's'); end
end
