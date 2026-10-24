%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = groupcounts(T, varargin)
  narginchk(1, Inf);
  if istable(T)
    nargoutchk(0, 1);
    if isempty(varargin)
      error('Nelson:data_analysis:groupingVariablesExpected', 'Grouping variables expected.');
    end
    groupVars = varargin{1};
    args = varargin(2:end);
    varargout{1} = groupcountsTable(T, groupVars, args);
  else
    [counts, groups, percent] = groupcountsArray(T, varargin);
    varargout{1} = counts;
    if nargout > 1
      varargout{2} = groups;
    end
    if nargout > 2
      varargout{3} = percent;
    end
  end
end
%=============================================================================
function G = groupcountsTable(T, groupVars, args)
  opts = parseGroupcountsOptions(args);
  if opts.GroupBinsProvided
    error('Nelson:data_analysis:groupBinsSupportedTableGroupcounts', 'Group bins are not supported for table groupcounts.');
  end
  st = struct(T);
  idx = tableResolveVariables(st.Properties.VariableNames, groupVars, st.Properties.VariableTypes, st.data);
  if isempty(idx)
    counts = height(T);
    percent = 100;
    G = table(counts, percent, 'VariableNames', {'GroupCount', 'Percent'});
    return
  end
  names = st.Properties.VariableNames;
  keys = tableRowKeys(st, height(T), idx);
  missingRows = any(ismissing(T(:, names(idx))), 2);
  if ~opts.IncludeMissingGroups
    keep = ~missingRows;
    keys = keys(keep);
    sourceRows = find(keep);
  else
    sourceRows = (1:height(T))';
  end
  [uniqueKeys, ia, ic] = tableUniqueStable(keys);
  rows = sourceRows(ia);
  counts = accumarray(ic(:), 1, [length(uniqueKeys), 1]);
  total = sum(counts);
  if total == 0
    percent = counts;
  else
    percent = counts * 100 / total;
  end
  G = T(rows, names(idx));
  G.GroupCount = counts;
  G.Percent = percent;
end
%=============================================================================
function [counts, groups, percent] = groupcountsArray(X, args)
  opts = parseGroupcountsOptions(args);
  if opts.GroupBinsProvided
    error('Nelson:data_analysis:groupBinsSupported', 'Group bins are not supported.');
  end
  if iscell(X)
    cols = cell(1, numel(X));
    for c = 1:numel(X)
      cols{c} = X{c}(:);
    end
    n = numel(X{1});
    groupSource = X;
  else
    n = size(X, 1);
    cols = cell(1, size(X, 2));
    for c = 1:size(X, 2)
      cols{c} = X(:, c);
    end
    groupSource = X;
  end
  keys = localArrayRowKeys(cols, n);
  [uniqueKeys, ia, ic] = tableUniqueStable(keys);
  counts = accumarray(ic(:), 1, [length(uniqueKeys), 1]);
  total = sum(counts);
  if total == 0
    percent = counts;
  else
    percent = counts * 100 / total;
  end
  if iscell(groupSource)
    groups = cell(1, length(groupSource));
    for c = 1:length(groupSource)
      groups{c} = groupSource{c}(ia);
    end
  else
    groups = groupSource(ia, :);
  end
end
%=============================================================================
function keys = localArrayRowKeys(cols, n)
  % Build one grouping key per row from the column list. Routed through the
  % hoisted tableRowKeys helper so array groupcounts does not call the private
  % tableValueKey once per row (per-row private dispatch is O(n) in Nelson,
  % which made this O(n^2)). The key strings differ from the former per-row
  % encoding but define the same equality classes, so grouping is unchanged.
  st = struct();
  st.data = struct();
  names = cell(1, numel(cols));
  for j = 1:numel(cols)
    names{j} = sprintf('v%d', j);
    st.data.(names{j}) = cols{j};
  end
  st.Properties.VariableNames = names;
  keys = tableRowKeys(st, n, 1:numel(cols));
end
%=============================================================================
function opts = parseGroupcountsOptions(args)
  opts = struct();
  opts.IncludeMissingGroups = true;
  opts.IncludeEmptyGroups = false;
  opts.IncludedEdge = 'left';
  opts.GroupBinsProvided = false;
  if ~isempty(args)
    first = args{1};
    if ~(ischar(first) || (isstring(first) && isscalar(first)))
      opts.GroupBinsProvided = true;
      args(1) = [];
    end
  end
  k = 1;
  while k <= length(args)
    name = args{k};
    if isstring(name)
      name = char(name);
    end
    if k == length(args)
      error('Nelson:data_analysis:nameValueArgumentFollowedValue', 'Name-value argument must be followed by a value.');
    end
    switch lower(name)
      case 'includemissinggroups'
        opts.IncludeMissingGroups = logical(args{k + 1});
      case 'includeemptygroups'
        opts.IncludeEmptyGroups = logical(args{k + 1});
      case 'includededge'
        opts.IncludedEdge = char(args{k + 1});
      otherwise
        error('Nelson:data_analysis:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
end
%=============================================================================
