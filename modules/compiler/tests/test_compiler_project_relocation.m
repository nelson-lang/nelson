%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 300-->
ncc('--help');
check_relocation();

function check_relocation()
  previous = pwd();
  work = [tempname(), ' relocated '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() cleanup_work(work, previous));
  original = fullfile(work, 'original');
  source = fullfile(original, 'source');
  mkdir(source);
  project = make_project(original, source);
  filename = fullfile(original, 'application.ncproj');
  __nelsonc_project__('save', project, filename);
  project = __nelsonc_project__('load', filename);
  reference = project;
  reference.Build.OutputDir = fullfile(work, 'reference');
  before = __nelsonc_project__('build', reference);
  digest = sha256(before.Files{1}, '-file');
  moved = fullfile(work, 'moved');
  [status, message] = movefile(original, moved);
  asserts.istrue(status, message);
  asserts.isfalse(isfolder(original));
  unrelated = fullfile(work, 'unrelated');
  mkdir(unrelated);
  cd(unrelated);
  unrelated = pwd();
  project = __nelsonc_project__('load', fullfile(moved, 'application.ncproj'));
  asserts.isfalse(contains(jsonencode(project), strrep(original, '\', '/')));
  plan = __nelsonc_project__('analyze', project);
  asserts.istrue(plan.complete);
  asserts.isfalse(any(ismember(plan.runtime.modules, {'compiler', 'image_processing'})));
  result = __nelsonc_project__('build', project);
  asserts.isequal(sha256(result.Files{1}, '-file'), digest);
  asserts.isequal(pwd(), unrelated);
  results = {result};
  if ispc()
    windowed = project;
    windowed.Target = 'standaloneWindowsApplication';
    windowed.Build.OutputDir = fullfile(moved, 'windowed');
    results{end + 1} = __nelsonc_project__('build', windowed);
    output = __nelsonc_project__('package', project, result);
    asserts.istrue(isfile(fullfile(output, 'MyAppInstaller.exe')));
  end
  rmdir(fullfile(moved, 'source'), 's');
  oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  for k = 1:numel(results)
    executable = results{k}.Files{1};
    [status, text] = system(['"', executable, '"'], 60);
    asserts.isequal(status, 0, text);
    asserts.istrue(contains(text, 'PROJECT_MOVED_OK:42:payload'), text);
    [status, text] = system(['"', executable, '" --help'], 60);
    asserts.isequal(status, 0, text);
    asserts.istrue(contains(text, 'PROJECT_MOVED_HELP'), text);
  end
  reopened = __nelsonc_project__('load', fullfile(moved, 'application.ncproj'));
  asserts.isequal(reopened.Build.AppFile, project.Build.AppFile);
  failed = false;
  try; __nelsonc_project__('analyze', reopened); catch; failed = true; end
  asserts.istrue(failed);
  asserts.isequal(pwd(), unrelated);
  clear restoreRuntime;
  clear cleanup;
end

function cleanup_work(work, previous)
  cd(previous);
  if isfolder(work); rmdir(work, 's'); end
end

function project = make_project(root, source)
  entry = fullfile(source, 'relocated_entry.m');
  lines = {'function relocated_entry()', ...
    'if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end', ...
    'data = fileread(fullfile(fileparts(mfilename(''fullpath'')), ''payload.txt''));', ...
    'disp([''PROJECT_MOVED_OK:'', int2str(relocated_helper()), '':'', data]);', 'end'};
  filewrite(entry, strjoin(lines, char(10)));
  filewrite(fullfile(source, 'relocated_helper.m'), 'function n = relocated_helper(); n = 42; end');
  filewrite(fullfile(source, 'payload.txt'), 'payload');
  filewrite(fullfile(source, 'help.txt'), 'PROJECT_MOVED_HELP');
  filewrite(fullfile(root, 'license.txt'), 'PROJECT_LICENSE');
  project = __nelsonc_project__('new');
  project.Build.AppFile = entry;
  project.Build.OutputDir = fullfile(root, 'build');
  project.Build.AdditionalFiles = {fullfile(source, 'payload.*')};
  project.Build.AutoDetectDataFiles = false;
  project.Build.CustomHelpTextFile = fullfile(source, 'help.txt');
  project.Installer.OutputDir = fullfile(root, 'installer');
  project.Installer.AdditionalFiles = {fullfile(root, 'license.txt')};
  if ispc()
    brand = fullfile(root, 'brand.png');
    copyfile(fullfile(modulepath('compiler'), 'examples', 'standalone', 'app_icon.png'), brand);
    project.Build.ExecutableIcon = brand;
    project.Installer.InstallerIcon = brand;
    project.Installer.InstallerLogo = brand;
    project.Installer.AddRemoveProgramsIcon = brand;
  end
end
