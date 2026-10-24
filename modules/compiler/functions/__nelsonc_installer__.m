%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_installer__(input, varargin)
  if isa(input, 'compiler.build.Results')
    if ~isscalar(input)
      error('Nelson:nelsonc:installer:applicationInstallerRequiresBuildResult', 'An application installer requires one build result.');
    end
    snapshot = input.__nelsonc_report__();
    report = jsondecode(__nelsonc_plan__('build-report', jsonencode(snapshot.Distribution)));
    files = input.Files;
    constructor = {input};
  else
    if isempty(varargin)
      error('Nelson:nelsonc:installer:applicationFilesRequireBuildresultJson', 'Application files require a buildresult.json report.');
    end
    reportFile = __nelsonc_option_value__('AdditionalFiles', varargin{1});
    if numel(reportFile) ~= 1 || ~isfile(reportFile{1})
      error('Nelson:nelsonc:installer:expectedExistingBuildReportFile', 'Expected one existing build report file.');
    end
    info = dir(reportFile{1});
    if info.bytes > 64 * 1024 * 1024
      error('Nelson:nelsonc:installer:buildReportTooLarge', 'Build report exceeds 64 MiB.');
    end
    report = jsondecode(__nelsonc_plan__('build-report', fileread(reportFile{1})));
    varargin(1) = [];
    files = input;
    constructor = {};
  end
  if numel(varargin) == 2 && (ischar(varargin{1}) || isstring(varargin{1})) ...
    && strcmp(varargin{1}, 'Options')
    options = varargin{2};
    if ~isa(options, 'compiler.package.InstallerOptions') || ~isscalar(options)
      error('Nelson:nelsonc:options:optionsInstallerOptionsObject', 'Options must be one InstallerOptions object.');
    end
  else
    options = compiler.package.InstallerOptions(constructor{:}, varargin{:});
  end
  if ~ispc() && strcmp(options.PackageType, 'zip')
    error('Nelson:nelsonc:platform:zipApplicationInstallersRequireWindows', 'ZIP application installers require Windows.');
  end
  if strcmp(options.RuntimeDelivery, 'web')
    error('Nelson:nelsonc:runtimeDownload', 'Automatic runtime download is unavailable. Use RuntimeDelivery installer or none.');
  end
  if ~strcmp(report.architecture, computer('arch'))
    error('Nelson:nelsonc:installer:buildReportArchitectureMismatch', 'The build report architecture differs from the packaging host.');
  end
  extensions = {'.exe', '.zip'};
  if ismac(); extensions = {'.pkg'}; elseif ~ispc(); extensions = {'.install'}; end
  for extension = extensions
    output = fullfile(options.OutputDir, [options.InstallerName, extension{1}]);
    if isfile(output) || isfolder(output)
      error('Nelson:nelsonc:output', 'Installer output already exists: %s.', output);
    end
  end
  if ispc(); engine = __nelsonc_inno_engine__(); end
  includeRuntime = strcmp(options.RuntimeDelivery, 'installer');
  plan = jsondecode(__nelsonc_plan__('distribution', jsonencode({report}), includeRuntime));
  inputs = __nelsonc_option_value__('AdditionalFiles', files);
  inputs = [inputs, options.AdditionalFiles];
  inventory = jsondecode(__nelsonc_plan__('installer-inputs', jsonencode(inputs)));
  stage = tempname();
  __application_archive__('directory', stage);
  cleanup = onCleanup(@() rmdir(stage, 's'));
  [payload, shortcut] = __nelsonc_installer_payload__(stage, inventory, report, options);
  if includeRuntime
    [parent, name] = fileparts(report.executable);
    runtime = fullfile(payload, parent, [name, '.runtime']);
    if isfolder(runtime) || isfile(runtime)
      error('Nelson:nelsonc:installer:applicationInputsCollideInstalledRuntime', 'Application inputs collide with the installed runtime.');
    end
    __application_archive__('directory', runtime);
    __nelsonc_copy_runtime__(plan.runtime, runtime);
    if ismac(); plan.runtime = __nelsonc_mac_relocate_runtime__(plan.runtime, runtime); end
  end
  if ispc()
    runtime = __nelsonc_windows_runtime_package__(engine, stage, payload, report, plan, options, inventory);
    images = __nelsonc_installer_images__(options, stage, payload);
    generator = @(output) __nelsonc_inno_script__(payload, output, shortcut, options, images, runtime);
    __nelsonc_inno_build__(engine, stage, payload, shortcut, options, generator);
  elseif ismac()
    __nelsonc_mac_installer__(stage, payload, shortcut, options, report);
  else
    __nelsonc_linux_installer__(stage, payload, shortcut, options, report, plan);
  end
  clear cleanup;
end
