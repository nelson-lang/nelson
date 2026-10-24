%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dendrogram(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 3);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseParentLocal(args);
  [Z, p, args] = parseTreeLocal(args);
  n = size(Z, 1) + 1;
  opts = parseOptionsLocal(args, n, max(Z(:, 3)));
  if ~isempty(opts.Parent)
    parent = opts.Parent;
  end
  nodes = treeNodesLocal(Z);
  minRank = nodeRanksLocal(nodes, n, opts.Reorder);
  displayNodes = displayNodesLocal(nodes, n, p, minRank);
  stop = false(1, 2 * n - 1);
  stop(displayNodes) = true;
  leafNodes = displayOrderLocal(2 * n - 1, nodes, n, stop, minRank);
  positions = zeros(1, 2 * n - 1);
  positions(leafNodes) = 1:numel(leafNodes);
  [H, leafNodes] = drawDendrogramLocal(parent, nodes, n, stop, minRank, positions, opts);
  T = leafAssignmentsLocal(leafNodes, nodes, n);
  outperm = outputPermutationLocal(leafNodes);
  if nargout >= 1
    varargout{1} = H;
  end
  if nargout >= 2
    varargout{2} = T;
  end
  if nargout >= 3
    varargout{3} = outperm;
  end
end
%=============================================================================
function [parent, args] = parseParentLocal(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'hggroup'))
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function [Z, p, args] = parseTreeLocal(args)
  if isempty(args)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  Z = args{1};
  if ~(isnumeric(Z) && isreal(Z) && ndims(Z) == 2 && size(Z, 2) == 3 && size(Z, 1) >= 1)
    error('Nelson:statistics:treeRealNumericMatrixThree', 'Tree must be a real numeric matrix with three columns.');
  end
  Z = double(Z);
  if any(~isfinite(Z(:))) || any(Z(:, 3) < 0)
    error('Nelson:statistics:treeContainFiniteNonnegativeDistances', 'Tree must contain finite nonnegative distances.');
  end
  validateTreeIdsLocal(Z);
  args = args(2:end);
  p = 30;
  if ~isempty(args) && isnumeric(args{1}) && isscalar(args{1}) && isreal(args{1})
    p = validatePValueLocal(args{1});
    args = args(2:end);
  end
end
%=============================================================================
function validateTreeIdsLocal(Z)
  n = size(Z, 1) + 1;
  for k = 1:size(Z, 1)
    ids = Z(k, 1:2);
    if any(floor(ids) ~= ids) || any(ids < 1) || any(ids >= n + k)
      error('Nelson:statistics:treeContainsInvalidNodeIdentifiers', 'Tree contains invalid node identifiers.');
    end
  end
end
%=============================================================================
function p = validatePValueLocal(p)
  if ~(isfinite(p) && p >= 0 && floor(p) == p)
    error('Nelson:statistics:pNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'P');
  end
  p = double(p);
end
%=============================================================================
function opts = parseOptionsLocal(args, n, maxHeight)
  opts = struct('Orientation', 'top', 'Labels', [], 'Reorder', [], ...
    'CheckCrossing', true, 'ColorThreshold', [], 'ShowCut', false, ...
    'ShowMarkers', false, 'ClusterIndices', [], 'Parent', []);
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = optionNameLocal(args{k});
    value = args{k + 1};
    switch name
      case 'orientation'
        opts.Orientation = orientationLocal(value);
      case 'labels'
        opts.Labels = labelsLocal(value, n);
      case 'reorder'
        opts.Reorder = reorderLocal(value, n);
      case 'checkcrossing'
        opts.CheckCrossing = logicalScalarLocal(value, 'CheckCrossing');
      case 'colorthreshold'
        opts.ColorThreshold = thresholdLocal(value, maxHeight);
      case 'showcut'
        opts.ShowCut = logicalScalarLocal(value, 'ShowCut');
      case 'showmarkers'
        opts.ShowMarkers = logicalScalarLocal(value, 'ShowMarkers');
      case 'clusterindices'
        opts.ClusterIndices = clusterIndicesLocal(value, n);
      case 'parent'
        opts.Parent = parentLocal(value);
    end
  end
end
%=============================================================================
function name = optionNameLocal(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
  end
  text = lower(char(value));
  names = {'reorder', 'checkcrossing', 'clusterindices', 'colorthreshold', ...
    'showcut', 'showmarkers', 'orientation', 'labels', 'parent'};
  hits = {};
  for k = 1:numel(names)
    if strncmp(names{k}, text, length(text))
      hits{end + 1} = names{k};
    end
  end
  if isempty(hits)
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  if numel(hits) > 1
    error('Nelson:statistics:ambiguousNameValue', 'Ambiguous name-value argument name.');
  end
  name = hits{1};
end
%=============================================================================
function value = orientationLocal(value)
  if ~(ischar(value) || isStringScalar(value))
    error('Nelson:statistics:orientationTopBottomLeftRight', 'Orientation must be ''top'', ''bottom'', ''left'', or ''right''.');
  end
  text = lower(char(value));
  choices = {'top', 'bottom', 'left', 'right'};
  hit = strcmp(text, choices);
  if ~any(hit)
    error('Nelson:statistics:orientationTopBottomLeftRight', 'Orientation must be ''top'', ''bottom'', ''left'', or ''right''.');
  end
  value = choices{hit};
end
%=============================================================================
function labels = labelsLocal(value, n)
  if iscellstr(value)
    labels = value(:);
  elseif isstring(value)
    labels = cellstr(value(:));
  elseif ischar(value)
    labels = cellstr(value);
  else
    error('Nelson:statistics:labelsCharacterStringCellString', 'Labels must be character, string, or cell string values.');
  end
  if numel(labels) ~= n
    error('Nelson:statistics:labelsContainOneValueObservation', 'Labels must contain one value per observation.');
  end
end
%=============================================================================
function reorder = reorderLocal(value, n)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && numel(value) == n)
    error('Nelson:statistics:reorderPermutationVector', 'Reorder must be a permutation vector.');
  end
  reorder = double(value(:)');
  if any(~isfinite(reorder)) || any(floor(reorder) ~= reorder) || ~isequal(sort(reorder), 1:n)
    error('Nelson:statistics:reorderPermutationVector', 'Reorder must be a permutation vector.');
  end
end
%=============================================================================
function tf = logicalScalarLocal(value, name)
  if islogical(value) && isscalar(value)
    tf = value ~= 0;
  elseif isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    tf = value ~= 0;
  else
    error('Nelson:validators:mustBeLogicalScalar', '%s must be a logical scalar.', name);
  end
end
%=============================================================================
function value = thresholdLocal(value, maxHeight)
  if (ischar(value) || isStringScalar(value)) && strcmpi(char(value), 'default')
    value = 0.7 * maxHeight;
  elseif isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value)
    value = double(value);
    if value <= 0 || value >= maxHeight
      value = [];
    end
  else
    error('Nelson:statistics:colorThresholdDefaultFiniteNumericScalar', 'ColorThreshold must be ''default'' or a finite numeric scalar.');
  end
end
%=============================================================================
function values = clusterIndicesLocal(value, n)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && numel(value) == n)
    error('Nelson:statistics:clusterIndicesContainOneIntegerObservation', 'ClusterIndices must contain one integer per observation.');
  end
  values = double(value(:));
  if any(~isfinite(values)) || any(floor(values) ~= values) || any(values < 1)
    error('Nelson:statistics:clusterIndicesContainPositiveIntegerValues', 'ClusterIndices must contain positive integer values.');
  end
end
%=============================================================================
function parent = parentLocal(value)
  if ~(isscalar(value) && (isgraphics(value, 'axes') || isgraphics(value, 'hggroup')))
    error('Nelson:statistics:parentAxesHggroupObject', 'Parent must be an axes or hggroup object.');
  end
  parent = value;
end
%=============================================================================
function nodes = treeNodesLocal(Z)
  n = size(Z, 1) + 1;
  count = 2 * n - 1;
  nodes.Left = zeros(1, count);
  nodes.Right = zeros(1, count);
  nodes.Height = zeros(1, count);
  nodes.Left(n + (1:n - 1)) = Z(:, 1)';
  nodes.Right(n + (1:n - 1)) = Z(:, 2)';
  nodes.Height(n + (1:n - 1)) = Z(:, 3)';
end
%=============================================================================
function minRank = nodeRanksLocal(nodes, n, reorder)
  if isempty(reorder)
    rank = 1:n;
  else
    rank = zeros(1, n);
    rank(reorder) = 1:n;
  end
  minRank = zeros(1, 2 * n - 1);
  minRank(1:n) = rank;
  for node = n + 1:2 * n - 1
    minRank(node) = min(minRank(nodes.Left(node)), minRank(nodes.Right(node)));
  end
end
%=============================================================================
function displayNodes = displayNodesLocal(nodes, n, p, minRank)
  root = 2 * n - 1;
  if p == 0 || n <= p
    target = n;
  else
    target = min(max(p, 1), n);
  end
  displayNodes = root;
  while numel(displayNodes) < target
    internal = displayNodes(displayNodes > n);
    if isempty(internal)
      break
    end
    [~, which] = max(nodes.Height(internal));
    node = internal(which);
    location = find(displayNodes == node, 1);
    kids = orderedChildrenLocal(node, nodes, minRank);
    displayNodes = [displayNodes(1:location - 1), kids, displayNodes(location + 1:end)];
  end
end
%=============================================================================
function kids = orderedChildrenLocal(node, nodes, minRank)
  kids = [nodes.Left(node), nodes.Right(node)];
  if minRank(kids(2)) < minRank(kids(1))
    kids = kids([2 1]);
  end
end
%=============================================================================
function order = displayOrderLocal(node, nodes, n, stop, minRank)
  if node <= n || stop(node)
    order = node;
  else
    kids = orderedChildrenLocal(node, nodes, minRank);
    order = [displayOrderLocal(kids(1), nodes, n, stop, minRank), ...
      displayOrderLocal(kids(2), nodes, n, stop, minRank)];
  end
end
%=============================================================================
function [H, leafNodes] = drawDendrogramLocal(parent, nodes, n, stop, minRank, positions, opts)
  if isempty(parent)
    parent = newplot();
  elseif isgraphics(parent, 'axes')
    parent = newplot(parent);
  end
  [H, leafNodes] = drawNodeLocal(parent, 2 * n - 1, nodes, n, stop, minRank, positions, opts);
  if opts.ShowMarkers
    H = [H; drawMarkersLocal(parent, numel(leafNodes), opts)];
  end
  if opts.ShowCut && ~isempty(opts.ColorThreshold)
    H = [H; drawCutLocal(parent, numel(leafNodes), opts)];
  end
  decorateAxesLocal(parent, nodes, leafNodes, n, opts);
end
%=============================================================================
function [H, leafNodes, x, y] = drawNodeLocal(parent, node, nodes, n, stop, minRank, positions, opts)
  if node <= n || stop(node)
    H = [];
    leafNodes = node;
    x = positions(node);
    y = 0;
    return
  end
  kids = orderedChildrenLocal(node, nodes, minRank);
  [HL, leavesL, xL, yL] = drawNodeLocal(parent, kids(1), nodes, n, stop, minRank, positions, opts);
  [HR, leavesR, xR, yR] = drawNodeLocal(parent, kids(2), nodes, n, stop, minRank, positions, opts);
  [xd, yd] = orientDataLocal([xL xL xR xR], [yL nodes.Height(node) nodes.Height(node) yR], opts);
  h = line(parent, xd, yd, 'Color', [0 0.4470 0.7410], 'LineWidth', 0.5);
  h.UserData = struct('Node', node, 'Children', kids, 'Height', nodes.Height(node));
  H = [HL; HR; h];
  leafNodes = [leavesL leavesR];
  x = (xL + xR) / 2;
  y = nodes.Height(node);
end
%=============================================================================
function h = drawMarkersLocal(parent, count, opts)
  [x, y] = orientDataLocal(1:count, zeros(1, count), opts);
  h = line(parent, x, y, 'LineStyle', 'none', 'Marker', 'o', ...
    'MarkerFaceColor', 'none', 'MarkerEdgeColor', [0 0 0]);
end
%=============================================================================
function h = drawCutLocal(parent, count, opts)
  t = opts.ColorThreshold;
  [x, y] = orientDataLocal([0.5 count + 0.5], [t t], opts);
  h = line(parent, x, y, 'LineStyle', '--', 'Color', [0.4 0.4 0.4]);
end
%=============================================================================
function [x, y] = orientDataLocal(x0, y0, opts)
  switch opts.Orientation
    case 'top'
      x = x0; y = y0;
    case 'bottom'
      x = x0; y = -y0;
    case 'left'
      x = y0; y = x0;
    case 'right'
      x = -y0; y = x0;
  end
end
%=============================================================================
function decorateAxesLocal(parent, nodes, leafNodes, n, opts)
  ax = parent;
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  end
  labels = tickLabelsLocal(leafNodes, n, opts.Labels);
  count = numel(leafNodes);
  maxHeight = max(nodes.Height);
  if any(strcmp(opts.Orientation, {'top', 'bottom'}))
    ax.XTick = 1:count;
    ax.XTickLabel = labels;
    xlim(ax, [0.5 count + 0.5]);
    ylim(ax, verticalLimitsLocal(maxHeight, opts.Orientation));
  else
    ax.YTick = 1:count;
    ax.YTickLabel = labels;
    ylim(ax, [0.5 count + 0.5]);
    xlim(ax, horizontalLimitsLocal(maxHeight, opts.Orientation));
  end
end
%=============================================================================
function labels = tickLabelsLocal(leafNodes, n, inputLabels)
  labels = cell(numel(leafNodes), 1);
  for k = 1:numel(leafNodes)
    node = leafNodes(k);
    if node <= n
      if isempty(inputLabels)
        labels{k} = num2str(node);
      else
        labels{k} = inputLabels{node};
      end
    else
      labels{k} = '';
    end
  end
end
%=============================================================================
function limits = verticalLimitsLocal(maxHeight, orientation)
  pad = max(maxHeight * 0.05, eps);
  if strcmp(orientation, 'bottom')
    limits = [-(maxHeight + pad), pad];
  else
    limits = [-pad, maxHeight + pad];
  end
end
%=============================================================================
function limits = horizontalLimitsLocal(maxHeight, orientation)
  pad = max(maxHeight * 0.05, eps);
  if strcmp(orientation, 'right')
    limits = [-(maxHeight + pad), pad];
  else
    limits = [-pad, maxHeight + pad];
  end
end
%=============================================================================
function T = leafAssignmentsLocal(leafNodes, nodes, n)
  T = zeros(n, 1);
  for k = 1:numel(leafNodes)
    leaves = descendantsLocal(leafNodes(k), nodes, n);
    T(leaves) = k;
  end
end
%=============================================================================
function leaves = descendantsLocal(node, nodes, n)
  if node <= n
    leaves = node;
  else
    leaves = [descendantsLocal(nodes.Left(node), nodes, n), ...
      descendantsLocal(nodes.Right(node), nodes, n)];
  end
end
%=============================================================================
function outperm = outputPermutationLocal(leafNodes)
  if all(leafNodes <= numel(leafNodes))
    outperm = leafNodes;
  else
    outperm = 1:numel(leafNodes);
  end
end
%=============================================================================
