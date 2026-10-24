%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitrtree(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, y] = __regression_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, size(X, 1), size(X, 2));
  tree = growTreeLocal(X, y, opts);
  mdl = RegressionTree();
  mdl.CutPredictor = tree.CutPredictor;
  mdl.CutPoint = tree.CutPoint;
  mdl.LeftChild = tree.LeftChild;
  mdl.RightChild = tree.RightChild;
  mdl.NodeMean = tree.NodeMean;
  mdl.NodeError = tree.NodeError;
  mdl.NodeSize = tree.NodeSize;
  mdl.IsBranch = tree.IsBranch;
  mdl.MaxNumSplits = opts.MaxNumSplits;
  mdl.MinLeafSize = opts.MinLeafSize;
  mdl.MinParentSize = opts.MinParentSize;
  mdl.PredictorNames = opts.PredictorNames;
  mdl.ResponseName = opts.ResponseName;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
  yfit = predict(mdl, X);
  mdl.ResubstitutionLoss = mean((y - yfit) .^ 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, n, p)
  opts = struct('MaxNumSplits', max(n - 1, 0), 'MinLeafSize', 1, 'MinParentSize', 10, ...
    'PredictorNames', {__default_predictor_names__(p)}, 'ResponseName', 'y');
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'maxnumsplits', 'minleafsize', 'minparentsize', ...
      'predictornames', 'responsename'});
    value = args{k + 1};
    switch name
      case 'maxnumsplits'
        opts.MaxNumSplits = nonnegativeIntegerLocal(value, 'MaxNumSplits');
      case 'minleafsize'
        opts.MinLeafSize = positiveIntegerLocal(value, 'MinLeafSize');
      case 'minparentsize'
        opts.MinParentSize = positiveIntegerLocal(value, 'MinParentSize');
      case 'predictornames'
        opts.PredictorNames = __predictor_names__(value, p);
      case 'responsename'
        opts.ResponseName = __response_name__(value);
    end
  end
end
%=============================================================================
function tree = growTreeLocal(X, y, opts)
  tree = emptyTreeLocal();
  tree = appendNodeLocal(tree, y);
  nodeRows = {1:length(y)};
  queue = 1;
  splits = 0;
  while ~isempty(queue) && splits < opts.MaxNumSplits
    node = queue(1);
    queue(1) = [];
    rows = nodeRows{node};
    split = bestSplitLocal(X(rows, :), y(rows), opts);
    if ~split.Valid
      continue
    end
    tree.IsBranch(node) = true;
    tree.CutPredictor(node) = split.Predictor;
    tree.CutPoint(node) = split.CutPoint;
    leftRows = rows(split.LeftMask);
    rightRows = rows(~split.LeftMask);
    tree = appendNodeLocal(tree, y(leftRows));
    leftNode = length(tree.IsBranch);
    tree = appendNodeLocal(tree, y(rightRows));
    rightNode = length(tree.IsBranch);
    tree.LeftChild(node) = leftNode;
    tree.RightChild(node) = rightNode;
    nodeRows{leftNode} = leftRows;
    nodeRows{rightNode} = rightRows;
    queue = [queue, leftNode, rightNode];
    splits = splits + 1;
  end
end
%=============================================================================
function tree = emptyTreeLocal()
  tree = struct('CutPredictor', [], 'CutPoint', [], 'LeftChild', [], 'RightChild', [], ...
    'NodeMean', [], 'NodeError', [], 'NodeSize', [], 'IsBranch', []);
end
%=============================================================================
function tree = appendNodeLocal(tree, y)
  tree.CutPredictor(end + 1, 1) = 0;
  tree.CutPoint(end + 1, 1) = NaN;
  tree.LeftChild(end + 1, 1) = 0;
  tree.RightChild(end + 1, 1) = 0;
  tree.NodeMean(end + 1, 1) = mean(y);
  tree.NodeError(end + 1, 1) = sseLocal(y);
  tree.NodeSize(end + 1, 1) = length(y);
  tree.IsBranch(end + 1, 1) = false;
end
%=============================================================================
function split = bestSplitLocal(X, y, opts)
  split = struct('Valid', false, 'Predictor', 0, 'CutPoint', NaN, 'LeftMask', [], 'Gain', 0);
  if length(y) < opts.MinParentSize || sseLocal(y) <= eps
    return
  end
  parentError = sseLocal(y);
  for p = 1:size(X, 2)
    candidate = bestPredictorSplitLocal(X(:, p), y, parentError, opts);
    if candidate.Valid && candidate.Gain > split.Gain
      split.Valid = true;
      split.Predictor = p;
      split.CutPoint = candidate.CutPoint;
      split.LeftMask = X(:, p) <= candidate.CutPoint;
      split.Gain = candidate.Gain;
    end
  end
end
%=============================================================================
function candidate = bestPredictorSplitLocal(x, y, parentError, opts)
  candidate = struct('Valid', false, 'CutPoint', NaN, 'Gain', 0);
  [xs, order] = sort(x(:));
  ys = y(order);
  boundary = find(diff(xs) ~= 0);
  if isempty(boundary)
    return
  end
  boundary = boundary(boundary >= opts.MinLeafSize & length(y) - boundary >= opts.MinLeafSize);
  if isempty(boundary)
    return
  end
  csum = cumsum(ys);
  csum2 = cumsum(ys .^ 2);
  leftN = boundary(:);
  rightN = length(y) - leftN;
  leftSum = csum(leftN);
  leftSum2 = csum2(leftN);
  rightSum = csum(end) - leftSum;
  rightSum2 = csum2(end) - leftSum2;
  leftError = max(leftSum2 - (leftSum .^ 2) ./ leftN, 0);
  rightError = max(rightSum2 - (rightSum .^ 2) ./ rightN, 0);
  [gain, idx] = max(parentError - leftError - rightError);
  if gain > eps
    pos = leftN(idx);
    candidate.Valid = true;
    candidate.CutPoint = (xs(pos) + xs(pos + 1)) / 2;
    candidate.Gain = gain;
  end
end
%=============================================================================
function value = sseLocal(y)
  value = sum((y - mean(y)) .^ 2);
end
%=============================================================================
function value = positiveIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value > 0 && value == fix(value))
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = nonnegativeIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && value >= 0 && value == fix(value))
    error('Nelson:statistics:mustBeNonnegativeInteger', '%s must be a nonnegative integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
