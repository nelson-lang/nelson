%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_runtime_installer__(name, input, varargin)
  name = __nelsonc_installer_option__('InstallerName', name);
  options = struct('InstallerName', name, 'OutputDir', fullpath(name), ...
    'RuntimeDelivery', 'web', 'PackageType', 'auto', 'OptionalDependencies', 'all', 'Compression', 'normal', 'Verbose', false);
  if mod(numel(varargin), 2) ~= 0
    error('Nelson:nelsonc:options:runtimeInstallerOptionsRequireName', 'Runtime installer options require name-value pairs.');
  end
  for k = 1:2:numel(varargin)
    key = varargin{k};
    if isstring(key) && isscalar(key)
      key = char(key);
    end
    if ~ischar(key) || ~isrow(key) || ~any(strcmp(key, ...
      {'OutputDir', 'RuntimeDelivery', 'PackageType', 'OptionalDependencies', 'Compression'}))
      error('Nelson:nelsonc:options:unknownRuntimeInstallerOption', 'Unknown runtime installer option.');
    end
    options.(key) = __nelsonc_installer_option__(key, varargin{k + 1});
  end
  if strcmp(options.RuntimeDelivery, 'none')
    error('Nelson:nelsonc:options:runtimeInstallerRequiresWebInstaller', 'A runtime installer requires web or installer delivery.');
  end
  if strcmp(options.PackageType, 'zip') && ~strcmp(options.RuntimeDelivery, 'installer')
    error('Nelson:nelsonc:options:zipRuntimeInstallersRequireInstallerDelivery', 'ZIP runtime installers require installer delivery.');
  end
  if ~ispc() && strcmp(options.PackageType, 'zip')
    error('Nelson:nelsonc:platform:zipRuntimeInstallersRequireWindows', 'ZIP runtime installers require Windows.');
  end
  if strcmp(options.RuntimeDelivery, 'web')
    error('Nelson:nelsonc:runtimeDownload:automaticRuntimeDownloadUnavailableInstallerOnly', 'Automatic runtime download is unavailable. Use RuntimeDelivery installer.');
  end
  extensions = {'.exe', '.zip'};
  if ismac(); extensions = {'.pkg'}; elseif ~ispc(); extensions = {'.install'}; end
  for extension = extensions
    output = fullfile(options.OutputDir, [name, extension{1}]);
    if isfile(output) || isfolder(output)
      error('Nelson:nelsonc:output', 'Installer output already exists: %s.', output);
    end
  end
  if ispc(); engine = __nelsonc_inno_engine__(); end
  plan = __nelsonc_distribution_plan__(input, ~ispc());
  if ~strcmp(plan.architecture, computer('arch'))
    error('Nelson:nelsonc:installer:buildReportArchitectureMismatch', 'The build report architecture differs from the packaging host.');
  end
  stage = tempname();
  __application_archive__('directory', stage);
  cleanup = onCleanup(@() rmdir(stage, 's'));
  payload = fullfile(stage, 'payload');
  __nelsonc_copy_runtime__(plan.runtime, payload);
  if ismac(); plan.runtime = __nelsonc_mac_relocate_runtime__(plan.runtime, payload); end
  if ispc()
    __nelsonc_runtime_inventory__(plan, fullfile(stage, 'runtime-package.ini'));
    script = @(output) __nelsonc_runtime_script__(stage, payload, output, plan, options);
    __nelsonc_inno_build__(engine, stage, payload, '', options, script, 'runtime');
  elseif ismac()
    __nelsonc_mac_runtime_installer__(stage, payload, plan, options);
  else
    __nelsonc_linux_runtime_installer__(stage, payload, plan, options);
  end
  clear cleanup;
end
