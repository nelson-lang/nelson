%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [payload, shortcut] = __nelsonc_installer_payload__(stage, inventory, report, options)
  if isempty(inventory.files) || ~any(same_path({inventory.files.relative}, report.executable))
    error('Nelson:nelsonc:installer:installerMissingRecordedExecutable', 'The installer must include the executable recorded in the build report.');
  end
  for k = 1:numel(report.files)
    recorded = report.files(k);
    found = find(same_path({inventory.files.relative}, recorded.relative));
    if ~isempty(found) && ~strcmp(inventory.files(found).sha256, recorded.sha256)
      error('Nelson:nelsonc:installer:builtApplicationFileChangedAfter', 'A built application file changed after compilation: %s.', recorded.relative);
    end
  end
  payload = fullfile(stage, 'payload');
  mkdir(payload);
  for k = 1:numel(inventory.directories)
    mkdir(fullfile(payload, inventory.directories{k}));
  end
  for k = 1:numel(inventory.files)
    item = inventory.files(k);
    destination = fullfile(payload, item.relative);
    parent = fileparts(destination);
    if ~isfolder(parent)
      mkdir(parent);
    end
    [ok, message] = copyfile(item.source, destination);
    if ~ok
      error('Nelson:nelsonc:installer', '%s', message);
    end
    if ~strcmp(sha256(destination, '-file'), item.sha256)
      error('Nelson:nelsonc:installer:installerInputChangedWhileStaging', 'Installer input changed while staging: %s.', item.source);
    end
  end
  actual = jsondecode(__nelsonc_executable__('launcher-contract', fullfile(payload, report.executable)));
  if ~isequal(actual, report.launcher)
    error('Nelson:nelsonc:installer:launcherContractMismatch', 'The executable launcher contract does not match the build report.');
  end
  __application_archive__('verify', fullfile(payload, report.executable));
  shortcut = '';
  if ~isempty(options.Shortcut)
    target = strrep(fullpath(options.Shortcut), '\', '/');
    for k = 1:numel(inventory.files)
      if same_path(target, inventory.files(k).source)
        shortcut = inventory.files(k).relative;
      end
    end
    for k = 1:numel(inventory.folders)
      if same_path(target, inventory.folders(k).source)
        shortcut = inventory.folders(k).relative;
      end
    end
    if isempty(shortcut)
      error('Nelson:nelsonc:installer:shortcutIdentifyFileFolderIncluded', 'Shortcut must identify a file or folder included in the installer.');
    end
  end
  filename = fullfile(payload, 'nelson-installation.txt');
  if isfile(filename) || isfolder(filename)
    error('Nelson:nelsonc:installer:applicationInputsCollideNelsonInstallation', 'Application inputs collide with nelson-installation.txt.');
  end
  lines = {options.ApplicationName, ['Version: ', options.Version], ...
    ['Author: ', options.AuthorName], ['Company: ', options.AuthorCompany], ...
    ['Contact: ', options.AuthorEmail], '', options.Summary, options.Description, '', ...
    options.InstallationNotes, '', ['Runtime delivery: ', options.RuntimeDelivery], ...
    ['Runtime version: ', report.runtimeVersion], ['Architecture: ', report.architecture], ...
    ['Required engine SHA-256: ', report.engineFingerprint], ...
    'For an external runtime, set NELSONC_RUNTIME_ROOT to a compatible installation.', ''};
  filewrite(filename, [char(65279), strjoin(lines, char(10))]);
end

function same = same_path(left, right)
  if ispc()
    same = strcmpi(left, right);
  else
    same = strcmp(left, right);
  end
end
