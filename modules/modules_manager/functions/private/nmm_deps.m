%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_deps(varargin)
  [args, jsonOutput] = nmm_json_option(varargin);
  if length(args) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  tree = nmm_tree(args{1});
  report = flattenDependencies(tree.dependencies, struct([]));
  report = nmm_json_output(report, jsonOutput);
end
%=============================================================================
function report = flattenDependencies(nodes, report)
  for k = 1:length(nodes)
    report = appendDependency(report, nodes(k));
    report = flattenDependencies(nodes(k).dependencies, report);
  end
end
%=============================================================================
function report = appendDependency(report, node)
  item = struct();
  item.package = node.package;
  item.version = node.version;
  item.requested = node.requested;
  item.installed = node.installed;
  item.action = node.action;
  if hasDependency(report, item.package, item.version, item.requested)
    return
  end
  if isempty(report)
    report = item;
  else
    report(end + 1) = item;
  end
end
%=============================================================================
function tf = hasDependency(report, packageName, packageVersion, requested)
  tf = false;
  for k = 1:length(report)
    if strcmp(report(k).package, packageName) ...
        && strcmp(report(k).version, packageVersion) ...
        && strcmp(report(k).requested, requested)
      tf = true;
      return
    end
  end
end
%=============================================================================
