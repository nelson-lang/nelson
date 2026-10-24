%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [groups, labels] = distributionPlotGroups(data, groupData)
  if ~isnumeric(data)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  if nargin < 2 || isempty(groupData)
    if isvector(data)
      groups = {double(data(:))};
    else
      groups = cell(1, size(data, 2));
      for k = 1:size(data, 2)
        groups{k} = double(data(:, k));
      end
    end
    labels = defaultLabels(numel(groups));
    return
  end
  values = double(data(:));
  if numel(values) ~= numel(groupData)
    error('Nelson:graphics:invalidGroupData', 'Group data must have the same number of elements as data.');
  end
  groupLabels = graphicsLabelsFromValue(groupData, numel(values));
  labels = {};
  index = zeros(numel(groupLabels), 1);
  for k = 1:numel(groupLabels)
    found = find(strcmp(groupLabels{k}, labels), 1);
    if isempty(found)
      labels{end + 1, 1} = groupLabels{k};
      found = numel(labels);
    end
    index(k) = found;
  end
  groups = cell(1, numel(labels));
  for k = 1:numel(labels)
    groups{k} = values(index == k);
  end
end
%=============================================================================
function labels = defaultLabels(n)
  labels = cell(n, 1);
  for k = 1:n
    labels{k} = num2str(k);
  end
end
%=============================================================================
