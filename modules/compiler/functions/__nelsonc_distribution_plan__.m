%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function plan = __nelsonc_distribution_plan__(input, requireLinuxDiscovery)
  reports = {};
  if isa(input, 'compiler.build.Results')
    if isempty(input) || ~isvector(input) || numel(input) > 64
      error('Nelson:nelsonc:distribution', 'One to 64 scalar build results are required.');
    end
    for k = 1:numel(input)
      current = input(k);
      report = current.__nelsonc_report__();
      if ~isfield(report, 'Distribution')
        error('Nelson:nelsonc:distribution:resultHasNoDistributionReport', 'The result has no distribution report; rebuild the application.');
      end
      reports{end + 1} = report.Distribution;
    end
  else
    filenames = report_filenames(input);
    for k = 1:numel(filenames)
      info = dir(filenames{k});
      if numel(info) ~= 1 || info.isdir || info.bytes > 64 * 1024 * 1024
        error('Nelson:nelsonc:distribution:buildReportTooLarge', 'A build report must be a regular file of at most 64 MiB.');
      end
      encoded = __nelsonc_plan__('build-report', fileread(filenames{k}));
      reports{end + 1} = jsondecode(encoded);
    end
  end
  if nargin() > 1 && requireLinuxDiscovery
    for k = 1:numel(reports)
      if reports{k}.launcher.runtimeDiscoveryVersion < 3
        error('Nelson:nelsonc:launcher', 'Rebuild applications with a launcher supporting shared runtime discovery.');
      end
    end
  end
  plan = jsondecode(__nelsonc_plan__('distribution', jsonencode(reports), true));
end

function filenames = report_filenames(input)
  if ischar(input)
    filenames = {input};
  elseif isstring(input)
    filenames = cellstr(input);
  elseif iscell(input)
    filenames = input;
    for k = 1:numel(filenames)
      if isstring(filenames{k}) && isscalar(filenames{k})
        filenames{k} = char(filenames{k});
      end
    end
  else
    error('Nelson:nelsonc:distribution:expectedBuildResultsBuildReport', 'Expected build results or build report filenames.');
  end
  if isempty(filenames) || ~isvector(filenames) || numel(filenames) > 64 || ~iscellstr(filenames)
    error('Nelson:nelsonc:distribution:buildReportFilenamesRequired', 'One to 64 build report filenames are required.');
  end
  for k = 1:numel(filenames)
    name = filenames{k};
    if isempty(name) || ~isrow(name) || any(name == char(0)) || ~isfile(name)
      error('Nelson:nelsonc:distribution:buildReportFileNotFound', 'Build report file does not exist.');
    end
    filenames{k} = fullpath(name);
  end
end
