%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitrensemble(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, y] = __regression_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, size(X, 1), size(X, 2));
  [learners, opts] = trainLearnersLocal(X, y, opts);
  mdl = RegressionEnsemble();
  mdl.Learners = learners;
  mdl.Method = opts.Method;
  mdl.NumLearningCycles = opts.NumLearningCycles;
  mdl.LearnerTemplate = opts.Learners;
  mdl.PredictorNames = opts.PredictorNames;
  mdl.ResponseName = opts.ResponseName;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
  mdl.LearnRate = opts.LearnRate;
  mdl.LearnerWeights = opts.LearnerWeights;
  mdl.InitialResponse = opts.InitialResponse;
  yfit = predict(mdl, X);
  mdl.ResubstitutionLoss = mean((y - yfit) .^ 2);
end
%=============================================================================
function opts = parseOptionsLocal(args, n, p)
  opts = struct('Method', 'Bag', 'Learners', 'tree', 'NumLearningCycles', 100, ...
    'MaxNumSplits', max(n - 1, 0), 'MinLeafSize', 1, 'MinParentSize', 10, ...
    'PredictorNames', {__default_predictor_names__(p)}, 'ResponseName', 'y', ...
    'LearnRate', 0.1, 'LearnerWeights', [], 'InitialResponse', 0);
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'method', 'learners', 'numlearningcycles', ...
      'maxnumsplits', 'minleafsize', 'minparentsize', 'predictornames', 'responsename', ...
      'learnrate'});
    value = args{k + 1};
    switch name
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
      case 'predictornames'
        opts.PredictorNames = __predictor_names__(value, p);
      case 'responsename'
        opts.ResponseName = __response_name__(value);
      case 'learnrate'
        opts.LearnRate = positiveScalarLocal(value, 'LearnRate');
    end
  end
end
%=============================================================================
function [learners, opts] = trainLearnersLocal(X, y, opts)
  if strcmpi(opts.Method, 'LSBoost')
    [learners, opts] = trainBoostLearnersLocal(X, y, opts);
    return
  end
  learners = cell(1, opts.NumLearningCycles);
  for m = 1:opts.NumLearningCycles
    rows = bootstrapRowsLocal(length(y), m);
    learners{m} = fitrtree(X(rows, :), y(rows), 'MaxNumSplits', opts.MaxNumSplits, ...
      'MinLeafSize', opts.MinLeafSize, 'MinParentSize', opts.MinParentSize, ...
      'PredictorNames', opts.PredictorNames, 'ResponseName', opts.ResponseName);
  end
end
%=============================================================================
function [learners, opts] = trainBoostLearnersLocal(X, y, opts)
  learners = cell(1, opts.NumLearningCycles);
  opts.InitialResponse = mean(y);
  prediction = opts.InitialResponse * ones(size(y));
  opts.LearnerWeights = opts.LearnRate * ones(1, opts.NumLearningCycles);
  for m = 1:opts.NumLearningCycles
    residual = y - prediction;
    learners{m} = fitrtree(X, residual, 'MaxNumSplits', opts.MaxNumSplits, ...
      'MinLeafSize', opts.MinLeafSize, 'MinParentSize', opts.MinParentSize, ...
      'PredictorNames', opts.PredictorNames, 'ResponseName', opts.ResponseName);
    prediction = prediction + opts.LearnerWeights(m) * predict(learners{m}, X);
  end
end
%=============================================================================
function rows = bootstrapRowsLocal(n, cycle)
  rows = mod((1:n)' * (cycle + 3) + cycle * 17, n) + 1;
end
%=============================================================================
function value = methodLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Method');
  end
  text = lower(char(value));
  choices = {'bag', 'lsboost'};
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidMethodValue', 'Invalid %s value.', 'Method');
  end
  if strcmp(matches{1}, 'lsboost')
    value = 'LSBoost';
  else
    value = 'Bag';
  end
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
function value = positiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', '%s must be a positive scalar.', name);
  end
  value = double(value);
end
%=============================================================================
