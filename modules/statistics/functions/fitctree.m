%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitctree(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, yIndex, classNames] = __classification_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, classNames, size(X, 1));
  [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, opts.ClassNames);
  prior = priorLocal(opts.Prior, yIndex, length(classNames));
  tree = growTreeLocal(X, yIndex, length(classNames), opts);
  mdl = ClassificationTree();
  mdl.ClassNames = classNames;
  mdl.Prior = prior;
  mdl.CutPredictor = tree.CutPredictor;
  mdl.CutPoint = tree.CutPoint;
  mdl.LeftChild = tree.LeftChild;
  mdl.RightChild = tree.RightChild;
  mdl.NodeClass = tree.NodeClass;
  mdl.NodeProbability = tree.NodeProbability;
  mdl.IsBranch = tree.IsBranch;
  mdl.SplitCriterion = opts.SplitCriterion;
  mdl.MaxNumSplits = opts.MaxNumSplits;
  mdl.MinLeafSize = opts.MinLeafSize;
  mdl.MinParentSize = opts.MinParentSize;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, classNames, n)
  opts = struct('ClassNames', {classNames}, 'Prior', 'empirical', 'SplitCriterion', 'gdi', ...
    'MaxNumSplits', max(n - 1, 0), 'MinLeafSize', 1, 'MinParentSize', 10);
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'classnames', 'prior', 'splitcriterion', 'maxnumsplits', ...
      'minleafsize', 'minparentsize'});
    value = args{k + 1};
    switch name
      case 'classnames'
        opts.ClassNames = __class_names__(value, classNames);
      case 'prior'
        opts.Prior = priorOptionLocal(value, length(classNames));
      case 'splitcriterion'
        opts.SplitCriterion = splitCriterionLocal(value);
      case 'maxnumsplits'
        opts.MaxNumSplits = nonnegativeIntegerLocal(value, 'MaxNumSplits');
      case 'minleafsize'
        opts.MinLeafSize = positiveIntegerLocal(value, 'MinLeafSize');
      case 'minparentsize'
        opts.MinParentSize = positiveIntegerLocal(value, 'MinParentSize');
    end
  end
end
%=============================================================================
function tree = growTreeLocal(X, yIndex, kClasses, opts)
  tree = emptyTreeLocal();
  tree = appendNodeLocal(tree, yIndex, kClasses);
  nodeRows = {1:length(yIndex)};
  queue = 1;
  splits = 0;
  while ~isempty(queue) && splits < opts.MaxNumSplits
    node = queue(1);
    queue(1) = [];
    rows = nodeRows{node};
    split = bestSplitLocal(X(rows, :), yIndex(rows), kClasses, opts);
    if ~split.Valid
      continue
    end
    tree.IsBranch(node) = true;
    tree.CutPredictor(node) = split.Predictor;
    tree.CutPoint(node) = split.CutPoint;
    leftRows = rows(split.LeftMask);
    rightRows = rows(~split.LeftMask);
    tree = appendNodeLocal(tree, yIndex(leftRows), kClasses);
    leftNode = length(tree.IsBranch);
    tree = appendNodeLocal(tree, yIndex(rightRows), kClasses);
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
    'NodeClass', [], 'NodeProbability', [], 'IsBranch', []);
end
%=============================================================================
function tree = appendNodeLocal(tree, labels, kClasses)
  counts = classCountsLocal(labels, kClasses);
  prob = counts / sum(counts);
  [~, cls] = max(prob);
  tree.CutPredictor(end + 1, 1) = 0;
  tree.CutPoint(end + 1, 1) = NaN;
  tree.LeftChild(end + 1, 1) = 0;
  tree.RightChild(end + 1, 1) = 0;
  tree.NodeClass(end + 1, 1) = cls;
  tree.NodeProbability(end + 1, :) = prob;
  tree.IsBranch(end + 1, 1) = false;
end
%=============================================================================
function split = bestSplitLocal(X, y, kClasses, opts)
  split = struct('Valid', false, 'Predictor', 0, 'CutPoint', NaN, 'LeftMask', [], 'Gain', 0);
  if length(y) < opts.MinParentSize || length(unique(y)) < 2
    return
  end
  parentImpurity = impurityLocal(classCountsLocal(y, kClasses), opts.SplitCriterion);
  for p = 1:size(X, 2)
    candidate = bestPredictorSplitLocal(X(:, p), y, kClasses, parentImpurity, opts);
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
function candidate = bestPredictorSplitLocal(x, y, kClasses, parentImpurity, opts)
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
  cumulative = cumulativeCountsLocal(ys, kClasses);
  parentCounts = cumulative(end, :);
  for pos = boundary(:).'
    leftCounts = cumulative(pos, :);
    rightCounts = parentCounts - leftCounts;
    gain = splitGainFromCountsLocal(leftCounts, rightCounts, parentImpurity, opts.SplitCriterion);
    if gain > candidate.Gain
      candidate.Valid = gain > 0;
      candidate.CutPoint = (xs(pos) + xs(pos + 1)) / 2;
      candidate.Gain = gain;
    end
  end
end
%=============================================================================
function cumulative = cumulativeCountsLocal(labels, kClasses)
  cumulative = zeros(length(labels), kClasses);
  for j = 1:kClasses
    cumulative(:, j) = cumsum(labels == j);
  end
end
%=============================================================================
function gain = splitGainFromCountsLocal(leftCounts, rightCounts, parentImpurity, criterion)
  nLeft = sum(leftCounts);
  nRight = sum(rightCounts);
  n = nLeft + nRight;
  leftImpurity = impurityLocal(leftCounts, criterion);
  rightImpurity = impurityLocal(rightCounts, criterion);
  gain = parentImpurity - (nLeft / n) * leftImpurity - (nRight / n) * rightImpurity;
end
%=============================================================================
function value = impurityLocal(counts, criterion)
  p = counts / sum(counts);
  p = p(p > 0);
  switch criterion
    case 'deviance'
      value = -sum(p .* log(p));
    otherwise
      value = 1 - sum(p .^ 2);
  end
end
%=============================================================================
function counts = classCountsLocal(labels, kClasses)
  counts = zeros(1, kClasses);
  for j = 1:kClasses
    counts(j) = sum(labels == j);
  end
end
%=============================================================================
function prior = priorLocal(option, yIndex, kClasses)
  if ischar(option)
    if strcmp(option, 'uniform')
      prior = ones(1, kClasses) / kClasses;
    else
      counts = classCountsLocal(yIndex, kClasses);
      prior = counts / sum(counts);
    end
  else
    prior = double(option(:).');
    prior = prior / sum(prior);
  end
end
%=============================================================================
function value = splitCriterionLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'SplitCriterion');
  end
  text = lower(char(value));
  names = {'gdi', 'deviance', 'twoing'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    value = exact{1};
    return
  end
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidSplitCriterionValue', 'Invalid %s value.', 'SplitCriterion');
  end
  value = matches{1};
end
%=============================================================================
function value = priorOptionLocal(value, kClasses)
  if ischar(value) || (isstring(value) && isscalar(value))
    text = lower(char(value));
    choices = {'empirical', 'uniform'};
    matches = choices(startsWith(choices, text));
    if length(matches) ~= 1
      error('Nelson:statistics:invalidPriorValue', 'Invalid %s value.', 'Prior');
    end
    value = matches{1};
  elseif isnumeric(value) && isreal(value) && isvector(value) && length(value) == kClasses ...
      && all(isfinite(value)) && all(value >= 0) && sum(value) > 0
    value = double(value(:).');
  else
    error('Nelson:statistics:invalidPriorValue', 'Invalid %s value.', 'Prior');
  end
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
