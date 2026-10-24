%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_update(varargin)
  if length(varargin) > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  dryRun = false;
  moduleName = '';
  for k = 1:length(varargin)
    value = varargin{k};
    if ~ischar(value)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', k + 2);
    end
    if strcmp(value, '-dry-run')
      dryRun = true;
    else
      if ~isempty(moduleName)
        nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
      end
      moduleName = value;
    end
  end
  if isempty(moduleName)
    outdated = nmm_outdated();
  else
    outdated = nmm_outdated(moduleName);
  end
  if dryRun
    report = struct();
    report.dry_run = true;
    report.updates = outdated;
    return
  end
  updated = {};
  for k = 1:length(outdated)
    moduleName = outdated(k).module;
    latestVersion = outdated(k).latest;
    nmm_uninstall(moduleName);
    nmm_install(moduleName, latestVersion);
    updated{end + 1} = moduleName;
  end
  report = struct();
  report.updated = updated;
end
%=============================================================================
