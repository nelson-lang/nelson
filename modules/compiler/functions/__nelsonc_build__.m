%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = __nelsonc_build__(entry, output, included, gui, runtimeMode, options)
  nargoutchk(0, 1);
  reportRequested = nargout() > 0;
  if nargin() < 6
    options = struct();
  end
  options = build_options(options);
  if nargin() < 5
    runtimeMode = 'bundled';
  end
  if ~ischar(runtimeMode) || ~any(strcmp(runtimeMode, {'bundled', 'installed'}))
    error('Nelson:nelsonc:arguments', 'Runtime mode must be bundled or installed.');
  end
  bundled = strcmp(runtimeMode, 'bundled');
  plan = __nelsonc_link_plan__(entry, included, options.autoDetectDataFiles, options.supportPackages);
  if ~plan.complete
    error('Nelson:nelsonc:dependencies', 'Unresolved dependencies: %s', jsonencode(plan.diagnostics));
  end
  gui = __nelsonc_select_mode__(plan, gui);
  plan = __nelsonc_application_native__(plan);
  launcher = 'nelson-app';
  if options.noConsole
    launcher = 'nelson-app-windowed';
  end
  stub = fullfile(modulepath('nelson', 'bin'), launcher);
  if ispc()
    stub = [stub, '.exe'];
  end
  if ~isfile(stub)
    error('Nelson:nelsonc:stub', 'The precompiled application launcher is missing.');
  end
  stubDigest = sha256(stub, '-file');
  if bundled
    runtimePlan = __nelsonc_runtime_plan__(plan, gui);
  elseif reportRequested
    runtimePlan = jsondecode(__nelsonc_plan__('runtime', jsonencode(plan), gui));
  end
  [outputParent, outputStem, outputExtension] = fileparts(output);
  if ispc() && ~strcmpi(outputExtension, '.exe')
    error('Nelson:nelsonc:output:windowsExecutableHaveExeExtension', 'A Windows executable must have the .exe extension.');
  end
  runtime = fullfile(outputParent, [outputStem, '.runtime']);
  externalArchive = fullfile(outputParent, [outputStem, '.nca']);
  if ~options.embedArchive && (isfile(externalArchive) || isfolder(externalArchive) || strcmp(output, externalArchive))
    error('Nelson:nelsonc:output:externalArchiveDestinationDistinctNot', 'External archive destination must be distinct and must not already exist.');
  end
  if isfile(output) || isfolder(output) || (bundled && (isfile(runtime) || isfolder(runtime)))
    error('Nelson:nelsonc:output:outputExecutableRuntimeNotAlready', 'Output executable and runtime must not already exist.');
  end
  if ~isfolder(outputParent)
    mkdir(outputParent);
  end
  stage = tempname();
  __application_archive__('directory', stage);
  cleanup = onCleanup(@() rmdir(stage, 's'));
  stubSnapshot = fullfile(stage, 'launcher');
  [success, message] = copyfile(stub, stubSnapshot);
  if ~success
    error('Nelson:nelsonc:copy', '%s: %s -> %s', message, stub, stubSnapshot);
  end
  if ~strcmpi(sha256(stubSnapshot, '-file'), stubDigest)
    error('Nelson:nelsonc:runtimeChanged:applicationLauncherChangedDuringPackaging', 'Application launcher changed during packaging.');
  end
  if ispc()
    __nelsonc_executable__('set-version', stubSnapshot, options.executableVersion);
  end
  plan.executableIconSHA256 = '';
  if ~isempty(options.executableIcon)
    [images, sides, plan.executableIconSHA256] = __nelsonc_icon_files__(options.executableIcon, stage);
    __nelsonc_executable__('set-icon', stubSnapshot, images);
  end
  launcherContract = jsondecode(__nelsonc_executable__('launcher-contract', stubSnapshot));
  if ~options.embedArchive && launcherContract.archiveMaxVersion < 3
    error('Nelson:nelsonc:stub:externalArchivesRequireRebuiltNative', 'External archives require rebuilt native application launchers.');
  end
  if ~isempty(options.runtimeLogFile)
    __nelsonc_executable__('set-log-file', stubSnapshot, options.runtimeLogFile);
  end
  expectedKind = 'console';
  if options.noConsole
    expectedKind = 'windowed';
  end
  if ~strcmp(launcherContract.kind, expectedKind)
    error('Nelson:nelsonc:stub:precompiledLauncherKindMismatch', 'The precompiled launcher does not match the requested application kind.');
  end
  payload = fullfile(stage, 'payload');
  mkdir(payload);
  manifest = __nelsonc_stage_application__(plan, payload, gui);
  manifest.runtimeMode = runtimeMode;
  if options.treatInputsAsNumeric
    manifest.format = 2;
    manifest.treatInputsAsNumeric = true;
  end
  if isfield(options, 'helpText')
    manifest.helpText = options.helpText;
  end
  write_text(fullfile(payload, 'application.json'), jsonencode(manifest));
  archive = fullfile(stage, 'application.zip');
  __nelsonc_archive__('pack', archive, {'application.json', 'roots'}, [payload, '/']);
  operation = 'create';
  if ~options.embedArchive; operation = 'create-external'; end
  if bundled
    __application_archive__('directory', runtime);
    try
      __nelsonc_copy_runtime__(runtimePlan, runtime);
      __nelsonc_archive__(operation, stubSnapshot, archive, output, gui);
    catch ex
      rmdir(runtime, 's');
      rethrow(ex);
    end
  else
    __nelsonc_archive__([operation, '-installed'], stubSnapshot, archive, output, gui, plan.engineFingerprint);
  end
  clear cleanup;
  if reportRequested
    runtimeDirectory = '';
    if bundled
      runtimeDirectory = runtime;
    end
    runtimePlan.mode = runtimeMode;
    runtimePlan.gui = gui;
    plan.noConsole = options.noConsole;
    plan.embedArchive = options.embedArchive;
    plan.treatInputsAsNumeric = options.treatInputsAsNumeric;
    plan.executableVersion = options.executableVersion;
    plan.executableIcon = options.executableIcon;
    plan.runtimeLogFile = options.runtimeLogFile;
    report = struct('Executable', output, 'RuntimeDirectory', runtimeDirectory, ...
      'DependencyPlan', plan, 'RuntimePlan', runtimePlan, 'Manifest', manifest, ...
      'SHA256', sha256(output, '-file'));
  end
end

function options = build_options(options)
  if ischar(options)
    options = struct('helpText', options);
  end
  if ~isstruct(options) || ~isscalar(options) ...
    || ~all(ismember(fieldnames(options), {'helpText', 'autoDetectDataFiles', 'noConsole', 'executableVersion', 'runtimeLogFile', 'executableIcon', 'embedArchive', 'treatInputsAsNumeric', 'supportPackages'}))
    error('Nelson:nelsonc:arguments:invalidApplicationBuildOptions', 'Invalid application build options.');
  end
  if ~isfield(options, 'autoDetectDataFiles')
    options.autoDetectDataFiles = true;
  end
  if ~isfield(options, 'embedArchive'); options.embedArchive = true; end
  if ~isfield(options, 'supportPackages'); options.supportPackages = {'autodetect'}; end
  options.supportPackages = __nelsonc_option_value__('SupportPackages', options.supportPackages);
  options.embedArchive = __nelsonc_option_value__('EmbedArchive', options.embedArchive);
  if ~isfield(options, 'treatInputsAsNumeric'); options.treatInputsAsNumeric = false; end
  options.treatInputsAsNumeric = __nelsonc_option_value__('TreatInputsAsNumeric', options.treatInputsAsNumeric);
  if ~isfield(options, 'executableVersion')
    options.executableVersion = '1.0.0.0';
  end
  options.executableVersion = __nelsonc_option_value__('ExecutableVersion', options.executableVersion);
  if ~isfield(options, 'executableIcon'); options.executableIcon = ''; end
  options.executableIcon = __nelsonc_option_value__('ExecutableIcon', options.executableIcon);
  if ~isfield(options, 'runtimeLogFile')
    options.runtimeLogFile = '';
  end
  options.runtimeLogFile = __nelsonc_option_value__('RuntimeLogFile', options.runtimeLogFile);
  if ~islogical(options.autoDetectDataFiles) || ~isscalar(options.autoDetectDataFiles)
    error('Nelson:nelsonc:arguments:automaticDataDetectionLogicalScalar', 'Automatic data detection must be a logical scalar.');
  end
  if ~isfield(options, 'noConsole')
    options.noConsole = false;
  end
  if ~islogical(options.noConsole) || ~isscalar(options.noConsole)
    error('Nelson:nelsonc:arguments:noConsoleModeLogicalScalar', 'No-console mode must be a logical scalar.');
  end
  if options.noConsole && ~ispc()
    error('Nelson:nelsonc:platform', 'No-console applications require Windows.');
  end
  if isfield(options, 'helpText')
    text = options.helpText;
    if ~ischar(text) || (~isrow(text) && ~isempty(text)) ...
      || numel(text) > 1048576 || any(text == char(0))
      error('Nelson:nelsonc:helpFile', 'Invalid application help text.');
    end
  end
end


function write_text(filename, text)
  fid = fopen(filename, 'wt', 'n', 'UTF-8');
  if fid < 0
    error('Nelson:nelsonc:write', 'Cannot create application manifest.');
  end
  closer = onCleanup(@() fclose(fid));
  fprintf(fid, '%s', text);
  clear closer;
end
