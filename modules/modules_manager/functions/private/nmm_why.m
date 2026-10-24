%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_why(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if length(args) == 1
    report = whyInstalled(args{1});
  elseif length(args) == 2
    report = whyPlan(args{1}, args{2});
  else
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  report = nmm_json_output(report, jsonOutput);
end
%=============================================================================
function report = whyInstalled(packageName)
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  modules = nmm_list();
  requiredBy = {};
  chains = {};
  installed = isfield(modules, packageName);
  for moduleName = fieldnames(modules)'
    requester = moduleName{1};
    versions = installedVersions(modules.(requester));
    for k = 1:length(versions)
      dependencies = installedDependencies(versions(k));
      if isfield(dependencies, packageName)
        requiredBy = appendUnique(requiredBy, requester);
        chains = appendUnique(chains, [requester, ' -> ', packageName]);
      end
    end
  end
  report = struct();
  report.package = packageName;
  report.installed = installed;
  report.required_by = requiredBy;
  report.direct = installed && isempty(requiredBy);
  report.chains = chains;
end
%=============================================================================
function report = whyPlan(planOrReport, packageName)
  if ~isstruct(planOrReport)
    nmm_error('StructExpected', '#%d argument: struct expected.', 2);
  end
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  plan = normalizedPlan(planOrReport);
  requiredBy = {};
  chains = {};
  inPlan = false;
  root = '';
  if isfield(planOrReport, 'package')
    root = planOrReport.package;
  elseif ~isempty(plan)
    root = plan(1).package;
  end
  for k = 1:length(plan)
    if strcmp(plan(k).package, packageName)
      inPlan = true;
    end
    dependencies = planDependencies(plan(k));
    if isfield(dependencies, packageName)
      requiredBy = appendUnique(requiredBy, plan(k).package);
      chains = appendUnique(chains, [plan(k).package, ' -> ', packageName]);
    end
  end
  if inPlan && isempty(chains) && ~isempty(root) && strcmp(root, packageName)
    chains = {packageName};
  end
  report = struct();
  report.package = packageName;
  report.installed = nmm_is_installed(packageName);
  report.in_plan = inPlan;
  report.required_by = requiredBy;
  report.direct = inPlan && isempty(requiredBy);
  report.chains = chains;
end
%=============================================================================
function plan = normalizedPlan(planOrReport)
  if isfield(planOrReport, 'install_plan')
    plan = planOrReport.install_plan;
  else
    plan = planOrReport;
  end
  if isempty(plan)
    plan = struct([]);
  end
end
%=============================================================================
function dependencies = planDependencies(item)
  dependencies = struct();
  if isfield(item, 'dependencies') && isstruct(item.dependencies)
    dependencies = item.dependencies;
  end
end
%=============================================================================
function dependencies = installedDependencies(versionData)
  dependencies = struct();
  if ~isfield(versionData, 'path')
    return
  end
  lockFile = [versionData.path, 'module-lock.json'];
  if ~isfile(lockFile)
    return
  end
  try
    lock = jsondecode(lockFile, '-file');
    if isfield(lock, 'dependencies') && isstruct(lock.dependencies)
      dependencies = lock.dependencies;
    end
  catch
  end
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
function values = appendUnique(values, value)
  if ~any(strcmp(values, value))
    values{end + 1} = value;
  end
end
%=============================================================================
