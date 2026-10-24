%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_doctor(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if length(args) > 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packageName = '';
  if length(args) == 1
    packageName = args{1};
    if ~ischar(packageName)
      nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
    end
  end
  if isempty(packageName)
    audit = nmm_audit();
  else
    audit = nmm_audit(packageName);
  end
  report = struct();
  report.ok = audit.ok;
  report.issues = audit.issues;
  if ~isempty(packageName)
    report.package = packageName;
    report.status = nmm_status(packageName);
  end
  report.nelson_version = nelsonVersionString();
  report.arch = computer('arch');
  report.abi = nelsonAbiTag();
  report.usermodulesdir = usermodulesdir();
  registry = nmm_registry_source();
  report.registry = registry;
  report.cache_dir = nmm_cache('dir');
  report.broken_modules = brokenModules(audit.issues);
  report.suggestions = suggestions(audit.issues, report.registry);
  report = nmm_json_output(report, jsonOutput);
end
%=============================================================================
function values = suggestions(issues, registry)
  values = {};
  if hasIssue(issues, 'installed path does not exist')
    values{end + 1} = 'run nmm(''repair'')';
  end
  if hasIssue(issues, 'module-lock.json checksum mismatch')
    values{end + 1} = 'reinstall the affected module';
  end
  if isfile(registry) && ~isfile([registry, '.sha256'])
    values{end + 1} = 'regenerate registry checksum';
  end
  try
    cacheReport = nmm_cache('verify');
    if ~cacheReport.ok
      values{end + 1} = 'run nmm(''cache'', ''prune'') or nmm(''cache'', ''clear'')';
    end
  catch
  end
end
%=============================================================================
function tf = hasIssue(issues, pattern)
  tf = false;
  for k = 1:length(issues)
    if contains(issues{k}, pattern)
      tf = true;
      return
    end
  end
end
%=============================================================================
function modules = brokenModules(issues)
  modules = {};
  for k = 1:length(issues)
    label = issues{k};
    separator = strfind(label, ':');
    if isempty(separator)
      continue
    end
    moduleLabel = label(1:separator(1) - 1);
    at = strfind(moduleLabel, '@');
    if ~isempty(at)
      moduleLabel = moduleLabel(1:at(1) - 1);
    end
    if ~any(strcmp(modules, moduleLabel))
      modules{end + 1} = moduleLabel;
    end
  end
end
%=============================================================================
function s = nelsonVersionString()
  v = version('-number');
  s = sprintf('%d.%d.%d', v(1), v(2), v(3));
end
%=============================================================================
function tag = nelsonAbiTag()
  compiler = version('-compiler');
  tag = [computer('arch'), '-', compiler{3}];
end
%=============================================================================
