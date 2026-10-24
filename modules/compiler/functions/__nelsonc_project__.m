%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [output, sourceStatus] = __nelsonc_project__(action, varargin)
  output = [];
  sourceStatus = [];
  switch action
    case 'new'
      output = new_project();
    case 'validate'
      output = validate_project(varargin{1});
    case 'load'
      filename = project_filename(varargin{1});
      info = dir(filename);
      if numel(info) ~= 1 || info.isdir || info.bytes > 1024 * 1024
        error('Nelson:compiler:project:fileTooLarge', 'Expected a project file no larger than 1 MiB.');
      end
      output = validate_project(jsondecode(fileread(filename)), fileparts(filename));
    case 'save'
      project = validate_project(varargin{1});
      filename = project_filename(varargin{2});
      replace = numel(varargin) == 3 && isequal(varargin{3}, true);
      if isfolder(filename) || (isfile(filename) && ~replace)
        error('Nelson:compiler:project:fileExists', 'Project file already exists.');
      end
      folder = fileparts(filename);
      if ~isfolder(folder); error('Nelson:compiler:project:directoryNotFound', 'Project directory does not exist.'); end
      temporary = [tempname(folder), '.ncproj'];
      cleanup = onCleanup(@() remove_temporary(temporary));
      if ~isempty(project.LastBuild.ReportFile) ...
        && ~strcmp(project.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', project))
        project.LastBuild = __nelsonc_project_result__('empty');
      end
      stored = __nelsonc_project_paths__(project, folder, true);
      if ~isempty(stored.LastBuild.ReportFile)
        stored.LastBuild.SettingsSHA256 = __nelsonc_project_result__('fingerprint', stored);
      end
      filewrite(temporary, jsonencode(stored));
      info = dir(temporary);
      if numel(info) ~= 1 || info.bytes > 1024 * 1024
        error('Nelson:compiler:project:fileTooLarge', 'Expected a project file no larger than 1 MiB.');
      end
      [status, message] = movefile(temporary, filename, 'f');
      if ~status; error('Nelson:compiler:project:saveFailed', '%s', message); end
      output = filename;
      clear cleanup;
    case 'options'
      output = build_options(validate_project(varargin{1}));
    case 'analyze'
      project = validate_project(varargin{1});
      options = build_options(project);
      native = nelson.compiler.BuildOptions(options.AppFile, rmfield(struct(options), 'AppFile'));
      native.RuntimeMode = 'installed';
      native.NoConsole = strcmp(project.Target, 'standaloneWindowsApplication');
      output = nelson.compiler.analyze(native);
    case 'build'
      project = validate_project(varargin{1});
      options = build_options(project);
      if strcmp(project.Target, 'standaloneWindowsApplication')
        output = compiler.build.standaloneWindowsApplication(options);
      else
        output = compiler.build.standaloneApplication(options);
      end
    case 'installer-options'
      project = validate_project(varargin{1});
      output = installer_options(project, varargin{2});
    case 'record-build'
      output = validate_project(varargin{1});
      output.LastBuild = __nelsonc_project_result__('record', output, varargin{2});
    case 'restore-build'
      output = __nelsonc_project_result__('restore', validate_project(varargin{1}));
    case 'source-status'
      output = __nelsonc_project_sources__(validate_project(varargin{1}));
    case 'package'
      project = validate_project(varargin{1});
      result = varargin{2};
      if ~isa(result, 'compiler.build.Results')
        result = __nelsonc_project_result__('restore', project);
        if isempty(result); error('Nelson:compiler:project:noSavedBuild', 'The project has no saved build.'); end
        sourceStatus = result.Sources;
      else
        sourceStatus = __nelsonc_project_sources__(project);
      end
      if strcmp(sourceStatus.State, 'changed')
        error('Nelson:compiler:staleBuild', '%s', sourceStatus.Message);
      end
      options = installer_options(project, result);
      if isa(result, 'compiler.build.Results')
        compiler.package.installer(result, 'Options', options);
      else
        __nelsonc_project_result__('package', result, options);
      end
      output = options.OutputDir;
    case 'command'
      output = __nelsonc_project_command__(validate_project(varargin{1}));
    otherwise
      error('Nelson:compiler:project:unknownOperation', 'Unknown project operation: %s.', action);
  end
end

function project = new_project()
  build = struct('AppFile', '', 'AdditionalFiles', {{}}, 'AutoDetectDataFiles', true, ...
    'CustomHelpTextFile', '', 'EmbedArchive', true, 'ExecutableName', '', ...
    'ExecutableIcon', '', 'ExecutableVersion', '1.0.0.0', 'OutputDir', '', ...
    'RuntimeLogFile', '', 'SupportPackages', {{'autodetect'}}, 'TreatInputsAsNumeric', false, 'Verbose', false);
  installer = struct(compiler.package.InstallerOptions('ApplicationName', 'Application'));
  installer.ApplicationName = '';
  installer.OutputDir = '';
  installer.DefaultInstallationDir = '';
  installer.RuntimeDelivery = 'none';
  project = struct('Format', 'nelson.compiler.project', 'Version', 6, ...
    'Target', 'standaloneApplication', 'Build', build, 'Installer', installer, ...
    'LastBuild', __nelsonc_project_result__('empty'));
end

function project = validate_project(project, folder)
  defaults = new_project();
  if isstruct(project) && isscalar(project) && isfield(project, 'Version') ...
    && (isequal(project.Version, 1) || isequal(project.Version, 2))
    check_fields(project, fieldnames(rmfield(defaults, 'LastBuild')));
    project.LastBuild = defaults.LastBuild;
  end
  check_fields(project, fieldnames(defaults));
  if isequal(project.Version, 3)
    check_fields(project.LastBuild, {'ReportFile'; 'ReportSHA256'; 'SettingsSHA256'});
    project.LastBuild.SourcesSHA256 = '';
  end
  legacy = isequal(project.Version, 1);
  if ~isequal(project.Format, defaults.Format) ...
    || (~legacy && ~isequal(project.Version, 2) && ~isequal(project.Version, 3) ...
    && ~isequal(project.Version, 4) && ~isequal(project.Version, 5) && ~isequal(project.Version, defaults.Version))
    error('Nelson:compiler:project:unsupportedFormat', 'Unsupported compiler project format or version.');
  end
  if ~ischar(project.Target) || ~any(strcmp(project.Target, ...
    {'standaloneApplication', 'standaloneWindowsApplication'}))
    error('Nelson:compiler:project:invalidTarget', 'Invalid standalone target.');
  end
  if strcmp(project.Target, 'standaloneWindowsApplication') && ~ispc()
    error('Nelson:compiler:project:targetRequiresWindows', 'The selected project target requires Windows.');
  end
  previousInputs = project.Version < 5;
  previousPackages = project.Version < 6;
  buildDefaults = defaults.Build;
  if previousInputs; buildDefaults = rmfield(buildDefaults, 'TreatInputsAsNumeric'); end
  if previousPackages; buildDefaults = rmfield(buildDefaults, 'SupportPackages'); end
  check_fields(project.Build, fieldnames(buildDefaults));
  check_fields(project.Installer, fieldnames(defaults.Installer));
  project.LastBuild = __nelsonc_project_result__('validate', project.LastBuild);
  restoreSignature = (nargin > 1 || previousPackages) && ~isempty(project.LastBuild.ReportFile) && strcmp(project.LastBuild.SettingsSHA256, ...
    __nelsonc_project_result__('fingerprint', project));
  if previousInputs
    project.Build.TreatInputsAsNumeric = false;
  end
  if previousPackages
    project.Build.SupportPackages = {'autodetect'};
    project.Build = orderfields(project.Build, defaults.Build);
  end
  if nargin > 1 && ~legacy
    project = __nelsonc_project_paths__(project, folder, false);
  end
  project.Version = defaults.Version;
  for section = {'Build', 'Installer'}
    name = section{1};
    values = project.(name);
    names = fieldnames(values);
    for k = 1:numel(names)
      key = names{k};
      value = values.(key);
      if strcmp(key, 'AdditionalFiles') && isempty(value); value = {}; end
      draft = any(strcmp(key, {'AppFile', 'ExecutableName', 'ApplicationName', 'OutputDir', 'DefaultInstallationDir'}));
      if draft && ischar(value) && isempty(value)
        values.(key) = '';
      else
        values.(key) = project_option(name, key, value);
      end
    end
    project.(name) = values;
  end
  if ~isempty(project.LastBuild.ReportFile)
    names = __nelsonc_option_value__('AdditionalFiles', project.LastBuild.ReportFile);
    project.LastBuild.ReportFile = names{1};
  end
  if restoreSignature
    project.LastBuild.SettingsSHA256 = __nelsonc_project_result__('fingerprint', project);
  end
  if strcmp(project.Installer.RuntimeDelivery, 'web')
    error('Nelson:compiler:project:webDeliveryUnavailable', 'Runtime web delivery is not available.');
  end
end

function value = project_option(section, key, value)
  sourcePath = any(strcmp(key, {'AppFile', 'CustomHelpTextFile', 'ExecutableIcon', ...
    'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon'}));
  if isstring(value) && isscalar(value); value = char(value); end
  % Projects can reopen missing sources so the user can repair their paths.
  % Public option objects still require those files before analysis or build.
  if sourcePath && ischar(value) && isrow(value) && ~isempty(value) && ~isfile(value) && ~isfolder(value)
    paths = __nelsonc_option_value__('AdditionalFiles', value);
    value = paths{1};
    [folder, name, extension] = fileparts(value);
    if strcmp(key, 'AppFile') && ~strcmp(extension, '.m')
      error('Nelson:compiler:project:appFileNotSource', 'AppFile must name a .m source file.');
    end
    if ~any(strcmp(key, {'AppFile', 'CustomHelpTextFile'})) && ...
      (~ispc() || ~any(strcmpi(extension, {'.jpg', '.jpeg', '.png', '.bmp', '.gif'})))
      error('Nelson:compiler:project:invalidImagePath', 'Invalid project image path or platform.');
    end
  elseif strcmp(section, 'Build')
    value = __nelsonc_option_value__(key, value);
  else
    value = __nelsonc_installer_option__(key, value);
  end
end

function check_fields(value, names)
  if ~isstruct(value) || ~isscalar(value) || ~isequal(sort(fieldnames(value)), sort(names))
    error('Nelson:compiler:project:invalidFields', 'Invalid compiler project fields.');
  end
end

function options = build_options(project)
  values = rmfield(project.Build, 'AppFile');
  for key = {'ExecutableName', 'OutputDir'}
    if isempty(values.(key{1})); values = rmfield(values, key{1}); end
  end
  options = compiler.build.StandaloneApplicationOptions(project.Build.AppFile);
  names = fieldnames(values);
  for k = 1:numel(names); options.(names{k}) = values.(names{k}); end
end

function options = installer_options(project, result)
  if isa(result, 'compiler.build.Results')
    options = compiler.package.InstallerOptions(result);
  else
    options = compiler.package.InstallerOptions('ApplicationName', result.Distribution.applicationName, ...
      'Shortcut', result.Files{1});
  end
  names = fieldnames(project.Installer);
  for k = 1:numel(names)
    key = names{k};
    value = project.Installer.(key);
    if isempty(value) && any(strcmp(key, {'ApplicationName', 'OutputDir', 'DefaultInstallationDir'})); continue; end
    options.(key) = value;
  end
end

function filename = project_filename(value)
  if isstring(value) && isscalar(value); value = char(value); end
  if ~ischar(value) || ~isrow(value) || any(value < char(32))
    error('Nelson:compiler:project:filenameExpected', 'Expected a project filename.');
  end
  values = __nelsonc_option_value__('AdditionalFiles', value);
  filename = values{1};
  [folder, name, extension] = fileparts(filename);
  if ~strcmpi(extension, '.ncproj')
    error('Nelson:compiler:project:extensionExpected', 'Compiler projects use the .ncproj extension.');
  end
end

function remove_temporary(filename)
  if isfile(filename); rmfile(filename); end
end
