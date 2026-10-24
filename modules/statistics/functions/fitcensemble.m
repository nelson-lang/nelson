%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitcensemble(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, yIndex, classNames, Yclean] = __classification_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, classNames, size(X, 1));
  [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, opts.ClassNames);
  Yclean = __labels_from_class_index__(classNames, yIndex, length(yIndex));
  learners = trainLearnersLocal(X, yIndex, Yclean, classNames, opts);
  mdl = ClassificationEnsemble();
  mdl.ClassNames = classNames;
  mdl.Learners = learners;
  mdl.Method = opts.Method;
  mdl.NumLearningCycles = opts.NumLearningCycles;
  mdl.LearnerTemplate = opts.Learners;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, classNames, n)
  opts = struct('ClassNames', {classNames}, 'Method', 'Bag', 'Learners', 'tree', ...
    'NumLearningCycles', 100, 'MaxNumSplits', max(n - 1, 0), 'MinLeafSize', 1, ...
    'MinParentSize', 10);
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'classnames', 'method', 'learners', ...
      'numlearningcycles', 'maxnumsplits', 'minleafsize', 'minparentsize'});
    value = args{k + 1};
    switch name
      case 'classnames'
        opts.ClassNames = __class_names__(value, classNames);
      case 'method'
        opts.Method = methodLocal(value);
      case 'learners'
        opts.Learners = learnersLocal(value);
      case 'numlearningcycles'
        opts.NumLearningCycles = positiveIntegerLocal(value, 'NumLearningCycles');
      case 'maxnumsplits'
        opts.MaxNumSplits = nonnegativeIntegerLocal(value, 'MaxNumSplits');
      case 'minleafsize'
        opts.MinLeafSize = positiveIntegerLocal(value, 'MinLeafSize');
      case 'minparentsize'
        opts.MinParentSize = positiveIntegerLocal(value, 'MinParentSize');
    end
  end
  if ~isequal(opts.ClassNames, classNames)
    __check_class_names__(classNames, opts.ClassNames, true);
  end
end
%=============================================================================
function learners = trainLearnersLocal(X, yIndex, Yclean, classNames, opts)
  learners = cell(1, opts.NumLearningCycles);
  n = size(X, 1);
  for m = 1:opts.NumLearningCycles
    rows = bootstrapRowsLocal(yIndex, m, length(classNames));
    learners{m} = fitctree(X(rows, :), Yclean(rows), 'ClassNames', classNames, ...
      'MaxNumSplits', opts.MaxNumSplits, 'MinLeafSize', opts.MinLeafSize, ...
      'MinParentSize', opts.MinParentSize);
  end
  n;
end
%=============================================================================
function rows = bootstrapRowsLocal(yIndex, cycle, kClasses)
  n = length(yIndex);
  rows = mod((1:n)' * (cycle + 3) + cycle * 17, n) + 1;
  if length(unique(yIndex(rows))) < kClasses
    rows = (1:n)';
  end
end
%=============================================================================
function value = methodLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Method');
  end
  text = lower(char(value));
  choices = {'bag'};
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidMethodValue', 'Invalid %s value.', 'Method');
  end
  value = 'Bag';
end
%=============================================================================
function value = learnersLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Learners');
  end
  text = lower(char(value));
  choices = {'tree'};
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidLearnersValue', 'Invalid %s value.', 'Learners');
  end
  value = 'tree';
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
