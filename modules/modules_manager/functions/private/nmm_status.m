%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_status(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packageName = varargin{1};
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  report = emptyStatus(packageName);
  modules = nmm_list();
  if ~isfield(modules, packageName)
    return
  end
  moduleRecord = modules.(packageName);
  [~, moduleData] = nmm_find_installed_module(packageName);
  versions = installedVersions(moduleRecord);
  report.installed = true;
  report.version = moduleData.version;
  report.versions = versionNames(versions);
  report.path = moduleData.path;
  report.autoload = moduleData.load;
  report.pinned = isfield(moduleRecord, 'pinned_version');
  if report.pinned
    report.pinned_version = moduleRecord.pinned_version;
  end
  report.lockfile = isfile([moduleData.path, 'module-lock.json']);
  tree = nmm_tree(packageName);
  report.dependencies = nmm_deps(packageName);
  report.missing = tree.missing;
  why = nmm_why(packageName);
  report.required_by = why.required_by;
  report.direct = why.direct;
  report.ok = report.lockfile && isempty(report.missing);
end
%=============================================================================
function report = emptyStatus(packageName)
  report = struct();
  report.package = packageName;
  report.installed = false;
  report.ok = false;
  report.version = '';
  report.versions = {};
  report.pinned = false;
  report.pinned_version = '';
  report.autoload = false;
  report.path = '';
  report.lockfile = false;
  report.dependencies = struct([]);
  report.missing = {};
  report.required_by = {};
  report.direct = false;
end
%=============================================================================
function versions = installedVersions(moduleData)
  versions = struct([]);
  if isfield(moduleData, 'versions') && ~isempty(moduleData.versions)
    versions = moduleData.versions;
    return
  end
  if isfield(moduleData, 'path') && isfield(moduleData, 'version')
    versions = moduleData;
    if isfield(versions, 'versions')
      versions = rmfield(versions, 'versions');
    end
    if isfield(versions, 'pinned_version')
      versions = rmfield(versions, 'pinned_version');
    end
  end
end
%=============================================================================
function names = versionNames(versions)
  names = {};
  for k = 1:length(versions)
    names{end + 1} = versions(k).version;
  end
end
%=============================================================================
