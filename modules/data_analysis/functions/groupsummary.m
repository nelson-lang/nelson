%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function G = groupsummary(T, groupVars, method, dataVars)
  narginchk(2, 4);
  if isa(T, 'timetable')
    % Grouping is by a data variable, so drop the row times and summarize
    % the data variables as a table (matching expected behavior).
    T = timetable2table(T, 'ConvertRowTimes', false);
  end
  mustBeA(T, 'table', 1);
  if nargin < 3 || isempty(method)
    method = 'count';
  end
  st = struct(T);
  names = st.Properties.VariableNames;
  if nargin < 4
    dataVars = setdiffIndicesLocal(1:width(T), tableResolveVariables(names, groupVars, st.Properties.VariableTypes, st.data));
  else
    dataVars = tableResolveVariables(names, dataVars, st.Properties.VariableTypes, st.data);
  end
  groupIdx = tableResolveVariables(names, groupVars, st.Properties.VariableTypes, st.data);
  keys = tableRowKeys(st, height(T), groupIdx);
  % Build the per-row group index in a single O(n log n) pass instead of
  % re-scanning all keys for every group.
  [uniqueKeys, ia, ic] = tableUniqueStable(keys);
  nGroups = length(uniqueKeys);
  rows = ia;
  groupCounts = accumarray(ic, 1, [nGroups, 1]);
  groupRows = groupRowIndices(ic, groupCounts);
  values = cell(1, length(dataVars));
  outNames = cell(1, length(dataVars));
  methodLabel = groupMethodName(method);
  for j = 1:length(dataVars)
    % Extract each data column once, then summarize each group.
    column = T.(names{dataVars(j)});
    outNames{j} = [methodLabel, '_', names{dataVars(j)}];
    for g = 1:nGroups
      data = tableColumnRows(column, groupRows{g});
      values{j} = appendGroupValue(values{j}, applyGroupMethod(data, method));
    end
  end
  G = T(rows, names(groupIdx));
  G.GroupCount = groupCounts;
  for j = 1:length(values)
    G.(outNames{j}) = values{j};
  end
end
%=============================================================================
function groupRows = groupRowIndices(ic, groupCounts)
  % Split 1:n row indices into per-group lists in stable, ascending order.
  if isempty(groupCounts)
    groupRows = cell(0, 1);
    return
  end
  [~, order] = sort(ic(:));
  groupRows = mat2cell(order, groupCounts, 1);
end
%=============================================================================
function idx = setdiffIndicesLocal(allIdx, removeIdx)
  idx = [];
  for k = allIdx(:)'
    if ~any(removeIdx == k)
      idx(end + 1) = k;
    end
  end
end
%=============================================================================
function nm = groupMethodName(method)
  if isa(method, 'function_handle')
    nm = 'fun1';
  else
    nm = char(method);
  end
end
%=============================================================================
function value = applyGroupMethod(data, method)
  if isa(method, 'function_handle')
    value = method(data);
    return
  end
  switch lower(char(method))
    case 'count'
      value = size(data, 1);
    case 'sum'
      value = sum(data);
    case 'mean'
      value = mean(data);
    case 'min'
      value = min(data);
    case 'max'
      value = max(data);
    otherwise
      error('Nelson:data_analysis:unsupportedSummaryMethod', 'Unsupported "%s" option.', char(method));
  end
end
%=============================================================================
function values = appendGroupValue(values, value)
  if isempty(values)
    values = value;
  else
    values = [values; value];
  end
end
%=============================================================================
