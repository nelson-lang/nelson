%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_explain(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packageName = varargin{1};
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  report = struct();
  report.package = packageName;
  report.status = nmm_status(packageName);
  report.tree = nmm_tree(packageName);
  report.why = nmm_why(packageName);
  report.summary = summaryLines(report.status, report.why, report.tree);
end
%=============================================================================
function lines = summaryLines(status, why, tree)
  lines = {};
  if ~status.installed
    lines{end + 1} = [status.package, ' is not installed.'];
    return
  end
  lines{end + 1} = [status.package, ' ', status.version, ' is installed.'];
  if status.pinned
    lines{end + 1} = ['Pinned version: ', status.pinned_version, '.'];
  end
  lines{end + 1} = ['Dependencies: ', int2str(length(status.dependencies)), '.'];
  if ~isempty(tree.missing)
    lines{end + 1} = ['Missing dependencies: ', strjoin(tree.missing, ', '), '.'];
  end
  if isempty(why.required_by)
    lines{end + 1} = 'No installed package depends on it.';
  else
    lines{end + 1} = ['Required by: ', strjoin(why.required_by, ', '), '.'];
  end
end
%=============================================================================
