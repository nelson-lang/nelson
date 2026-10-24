%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function output = __nelsonc_project_result__(action, project, varargin)
  switch action
    case 'empty'
      output = struct('ReportFile', '', 'ReportSHA256', '', 'SettingsSHA256', '', 'SourcesSHA256', '');
    case 'fingerprint'
      output = sha256(jsonencode(struct('Target', project.Target, 'Build', orderfields(project.Build))));
    case 'validate'
      output = validate_receipt(project);
    case 'record'
      output = record_build(project, varargin{1});
    case 'restore'
      output = restore_build(project);
    case 'package'
      package_build(project, varargin{1});
      output = [];
    otherwise
      error('Nelson:compiler:project:unknownResultOperation', 'Unknown project result operation.');
  end
end

function receipt = validate_receipt(receipt)
  expected = {'ReportFile'; 'ReportSHA256'; 'SettingsSHA256'; 'SourcesSHA256'};
  if ~isstruct(receipt) || ~isscalar(receipt) || ~isequal(sort(fieldnames(receipt)), sort(expected))
    error('Nelson:compiler:project:invalidSavedBuildReference', 'Invalid saved build reference.');
  end
  for k = 1:numel(expected)
    key = expected{k};
    value = receipt.(key);
    if isstring(value) && isscalar(value); value = char(value); end
    if ~ischar(value) || (~isempty(value) && ~isrow(value)) || any(value < char(32))
      error('Nelson:compiler:project:invalidSavedBuildReferenceValue', 'Invalid saved build reference value.');
    end
    receipt.(key) = value;
  end
  if isempty(receipt.ReportFile)
    if ~isempty(receipt.ReportSHA256) || ~isempty(receipt.SettingsSHA256) || ~isempty(receipt.SourcesSHA256)
      error('Nelson:compiler:project:incompleteSavedBuildReference', 'Incomplete saved build reference.');
    end
    return;
  end
  if numel(receipt.ReportFile) > 32768
    error('Nelson:compiler:project:savedReportPathTooLong', 'Saved build report path is too long.');
  end
  for key = {'ReportSHA256', 'SettingsSHA256', 'SourcesSHA256'}
    if strcmp(key{1}, 'SourcesSHA256') && isempty(receipt.SourcesSHA256); continue; end
    if isempty(regexp(receipt.(key{1}), '^[0-9a-f]{64}$', 'once'))
      error('Nelson:compiler:project:invalidSavedBuildFingerprint', 'Invalid saved build fingerprint.');
    end
  end
end

function receipt = record_build(project, result)
  if ~isa(result, 'compiler.build.Results') || ~isscalar(result) || ~strcmp(result.BuildType, project.Target)
    error('Nelson:compiler:project:currentBuildResultExpected', 'Expected the current project build result.');
  end
  options = __nelsonc_project__('options', project);
  if ~isequal(struct(options), struct(result.Options))
    error('Nelson:compiler:project:buildResultMismatch', 'The build result does not match the project settings.');
  end
  filename = fullfile(result.Options.OutputDir, 'buildresult.json');
  receipt = struct('ReportFile', filename, 'ReportSHA256', sha256(filename, '-file'), ...
    'SettingsSHA256', __nelsonc_project_result__('fingerprint', project), 'SourcesSHA256', '');
  document = read_report(receipt);
  snapshot = result.__nelsonc_report__();
  expected = jsondecode(__nelsonc_plan__('build-report', jsonencode(snapshot.Distribution)));
  if ~isequal(document, expected)
    error('Nelson:compiler:project:savedReportDiffers', 'The saved report differs from the build result.');
  end
  verify_outputs(document, filename);
  if isfield(snapshot.DependencyPlan, 'executableIconSHA256')
    helpText = '';
    if isfield(snapshot.Manifest, 'helpText'); helpText = snapshot.Manifest.helpText; end
    receipt.SourcesSHA256 = __nelsonc_source_fingerprint__(snapshot.DependencyPlan, ...
      helpText, snapshot.DependencyPlan.executableIconSHA256);
  end
end

function result = restore_build(project)
  result = [];
  receipt = validate_receipt(project.LastBuild);
  if isempty(receipt.ReportFile); return; end
  if ~strcmp(receipt.SettingsSHA256, __nelsonc_project_result__('fingerprint', project))
    error('Nelson:compiler:project:buildSettingsChanged', 'Build settings changed; rebuild before creating an installer.');
  end
  document = read_report(receipt);
  if ~strcmp(document.buildType, project.Target) || ~strcmp(document.architecture, computer('arch'))
    error('Nelson:compiler:project:savedBuildTargetMismatch', 'The saved build target or architecture does not match this project.');
  end
  files = verify_outputs(document, receipt.ReportFile);
  result = struct('Files', {files}, 'ReportFile', receipt.ReportFile, ...
    'Distribution', document, 'Sources', __nelsonc_project_sources__(project));
end

function document = read_report(receipt)
  filename = receipt.ReportFile;
  info = dir(filename);
  if numel(info) ~= 1 || info.isdir || info.bytes > 64 * 1024 * 1024 || ~isfile(filename)
    error('Nelson:compiler:project:savedReportMissingOrTooLarge', 'The saved build report is missing or exceeds 64 MiB.');
  end
  if ~strcmp(sha256(filename, '-file'), receipt.ReportSHA256)
    error('Nelson:compiler:project:savedReportChanged', 'The saved build report changed; rebuild before creating an installer.');
  end
  document = jsondecode(__nelsonc_plan__('build-report', fileread(filename)));
  if ~strcmp(sha256(filename, '-file'), receipt.ReportSHA256)
    error('Nelson:compiler:project:reportChangedWhileRestoring', 'The build report changed while restoring the project.');
  end
end

function files = verify_outputs(document, filename)
  folder = fileparts(filename);
  entries = document.files;
  if numel(entries) > 64
    error('Nelson:compiler:project:tooManySavedOutputs', 'Too many saved application outputs.');
  end
  files = cell(numel(entries), 1);
  for k = 1:numel(entries)
    files{k} = fullfile(folder, entries(k).relative);
    if ~isfile(files{k})
      error('Nelson:compiler:project:savedOutputMissing', 'A saved application output is missing: %s.', entries(k).relative);
    end
  end
  inventory = jsondecode(__nelsonc_plan__('installer-inputs', jsonencode(files)));
  for k = 1:numel(entries)
    found = find(strcmp({inventory.files.relative}, entries(k).relative));
    if numel(found) ~= 1 || ~strcmp(inventory.files(found).sha256, entries(k).sha256)
      error('Nelson:compiler:project:savedOutputChanged', 'A saved application output changed: %s.', entries(k).relative);
    end
  end
  index = find(strcmp({entries.relative}, document.executable));
  executable = files{index};
  files(index) = [];
  files = [{executable}; files];
  contract = jsondecode(__nelsonc_executable__('launcher-contract', executable));
  if ~isequal(contract, document.launcher)
    error('Nelson:compiler:project:launcherContractMismatch', 'The saved executable has a different launcher contract.');
  end
  __application_archive__('verify', executable);
end

function package_build(result, options)
  stage = tempname();
  __application_archive__('directory', stage);
  cleanup = onCleanup(@() rmdir(stage, 's'));
  filename = fullfile(stage, 'buildresult.json');
  % Package the validated report snapshot, not a file that can change after restore.
  filewrite(filename, jsonencode(result.Distribution));
  compiler.package.installer(result.Files, filename, 'Options', options);
  clear cleanup;
end
