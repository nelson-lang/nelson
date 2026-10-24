%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function encoded = __nelsonc_previous_build__(options, outputs)
  encoded = '[]';
  if isfolder(options.OutputDir)
    __nelsonc_executable__('recover-publication', options.OutputDir);
  end
  filename = fullfile(options.OutputDir, 'buildresult.json');
  names = {};
  if isfile(filename)
    info = dir(filename);
    if numel(info) ~= 1 || info.bytes > 64 * 1024 * 1024
      error('Nelson:nelsonc:output:previousBuildReportTooLarge', 'The previous build report exceeds 64 MiB.');
    end
    digest = sha256(filename, '-file');
    report = jsondecode(__nelsonc_plan__('build-report', fileread(filename)));
    if ~strcmp(report.applicationName, options.ExecutableName) || ~strcmp(report.architecture, computer('arch'))
      error('Nelson:nelsonc:output:outputDirectoryBelongsAnotherApplication', 'The output directory belongs to another application or architecture.');
    end
    previous = report.files(:);
    suffix = '';
    if ispc(); suffix = '.exe'; end
    executable = [options.ExecutableName, suffix];
    allowed = {executable, 'readme.txt', [options.ExecutableName, '.nca']};
    if ~strcmp(report.executable, executable) || ~any(strcmp({previous.relative}, 'readme.txt')) ...
      || ~all(ismember({previous.relative}, allowed))
      error('Nelson:nelsonc:output:reportNotStandaloneOutput', 'The report does not describe a standalone output directory.');
    end
    previous(end + 1) = struct('relative', 'buildresult.json', 'sha256', digest);
    if numel(previous) > 64
      error('Nelson:nelsonc:output:tooManyPreviousApplicationOutputs', 'Too many previous application outputs.');
    end
    files = cell(numel(previous), 1);
    for k = 1:numel(previous)
      name = previous(k).relative;
      [parent, stem, extension] = fileparts(name);
      if ~isempty(parent) || strcmp(name, '.nelsonc-publish')
        error('Nelson:nelsonc:output:previousOutputsOrdinaryFilesOutput', 'Previous outputs must be ordinary files in the output directory.');
      end
      files{k} = fullfile(options.OutputDir, name);
      if ~isfile(files{k})
        error('Nelson:nelsonc:output:previousApplicationOutputMissing', 'A previous application output is missing: %s.', name);
      end
    end
    inventory = jsondecode(__nelsonc_plan__('installer-inputs', jsonencode(files)));
    for k = 1:numel(previous)
      found = find(strcmp({inventory.files.relative}, previous(k).relative));
      if numel(found) ~= 1 || ~strcmp(inventory.files(found).sha256, previous(k).sha256)
        error('Nelson:nelsonc:output:previousApplicationOutputChanged', 'A previous application output changed: %s.', previous(k).relative);
      end
    end
    names = files;
    encoded = jsonencode(previous);
  end
  for k = 1:numel(outputs)
    if (isfile(outputs{k}) || isfolder(outputs{k})) && ~any(strcmp(outputs{k}, names))
      error('Nelson:nelsonc:output:unrelatedFileOccupiesApplicationOutput', 'An unrelated file occupies an application output: %s.', outputs{k});
    end
  end
end
