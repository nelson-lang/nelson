%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_tree(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if length(args) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  value = args{1};
  if ischar(value)
    report = installedTree(value);
  elseif isstruct(value)
    report = planTree(value);
  else
    nmm_error('InvalidArgumentType', '#%d argument: characters or struct expected.', 2);
  end
  report = nmm_json_output(report, jsonOutput);
end
%=============================================================================
function report = installedTree(packageName)
  [~, moduleData] = nmm_find_installed_module(packageName);
  if isempty(moduleData)
    report = missingNode(packageName, '');
    return
  end
  report = installedNode(packageName, moduleData, {});
end
%=============================================================================
function node = installedNode(packageName, moduleData, stack)
  node = emptyNode(packageName, moduleData.version);
  node.installed = true;
  node.action = 'installed';
  if any(strcmp(stack, packageName))
    node.conflicts = {[strjoin([stack, {packageName}], ' -> '), ' cycle']};
    return
  end
  dependencies = lockDependencies(moduleData);
  stack{end + 1} = packageName;
  for dependency = fieldnames(dependencies)'
    dependencyName = dependency{1};
    dependencyVersion = dependencies.(dependencyName);
    [~, dependencyData] = nmm_find_installed_module(dependencyName, dependencyVersion);
    if isempty(dependencyData)
      child = missingNode(dependencyName, dependencyVersion);
    else
      child = installedNode(dependencyName, dependencyData, stack);
      child.requested = dependencyVersion;
    end
    node = appendChild(node, child);
  end
end
%=============================================================================
function report = planTree(planOrReport)
  plan = normalizedPlan(planOrReport);
  if isempty(plan)
    nmm_error('InstallPlanEmpty', 'Install plan is empty.');
  end
  if isfield(planOrReport, 'install_plan') && isfield(planOrReport, 'package')
    root = planOrReport.package;
  else
    root = plan(1).package;
  end
  report = planNode(root, plan, {});
  if isfield(planOrReport, 'conflicts') && iscell(planOrReport.conflicts)
    report.conflicts = mergeValues(report.conflicts, planOrReport.conflicts);
  end
end
%=============================================================================
function node = planNode(packageName, plan, stack)
  item = findPlanItem(plan, packageName);
  if isempty(item)
    [~, moduleData] = nmm_find_installed_module(packageName);
    if isempty(moduleData)
      node = missingNode(packageName, '');
    else
      node = installedNode(packageName, moduleData, stack);
    end
    return
  end
  node = emptyNode(item.package, item.version);
  node.installed = nmm_is_installed(item.package, item.version);
  node.action = item.action;
  if isfield(item, 'requested')
    node.requested = item.requested;
  end
  if any(strcmp(stack, item.package))
    node.conflicts = {[strjoin([stack, {item.package}], ' -> '), ' cycle']};
    return
  end
  dependencies = planDependencies(item);
  stack{end + 1} = item.package;
  for dependency = fieldnames(dependencies)'
    dependencyName = dependency{1};
    child = planNode(dependencyName, plan, stack);
    child.requested = dependencies.(dependencyName);
    node = appendChild(node, child);
  end
end
%=============================================================================
function node = emptyNode(packageName, packageVersion)
  node = struct();
  node.package = packageName;
  node.version = packageVersion;
  node.installed = false;
  node.requested = '';
  node.action = '';
  node.dependencies = struct([]);
  node.missing = {};
  node.conflicts = {};
end
%=============================================================================
function node = missingNode(packageName, requested)
  node = emptyNode(packageName, '');
  node.requested = requested;
  node.action = 'missing';
  node.missing = {missingLabel(packageName, requested)};
end
%=============================================================================
function node = appendChild(node, child)
  if isempty(node.dependencies)
    node.dependencies = child;
  else
    node.dependencies(end + 1) = child;
  end
  node.missing = mergeValues(node.missing, child.missing);
  node.conflicts = mergeValues(node.conflicts, child.conflicts);
end
%=============================================================================
function item = findPlanItem(plan, packageName)
  item = struct([]);
  for k = 1:length(plan)
    if strcmp(plan(k).package, packageName)
      item = plan(k);
      return
    end
  end
end
%=============================================================================
function dependencies = lockDependencies(moduleData)
  dependencies = struct();
  if ~isfield(moduleData, 'path')
    return
  end
  lockFile = [moduleData.path, 'module-lock.json'];
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
function values = mergeValues(values, extra)
  for k = 1:length(extra)
    if ~any(strcmp(values, extra{k}))
      values{end + 1} = extra{k};
    end
  end
end
%=============================================================================
function label = missingLabel(packageName, requested)
  label = packageName;
  if ~isempty(requested)
    label = [packageName, '@', requested];
  end
end
%=============================================================================
