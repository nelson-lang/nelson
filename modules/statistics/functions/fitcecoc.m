%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mdl = fitcecoc(X, Y, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 1);
  [X, yIndex, classNames, Yclean] = __classification_training_data__(X, Y);
  if rem(length(varargin), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  opts = parseOptionsLocal(varargin, X, classNames);
  [yIndex, classNames] = __reorder_class_names__(yIndex, classNames, opts.ClassNames);
  learners = trainLearnersLocal(X, yIndex, classNames, Yclean, opts);
  mdl = ClassificationECOC();
  mdl.ClassNames = classNames;
  mdl.BinaryLearners = learners.BinaryLearners;
  mdl.CodingMatrix = learners.CodingMatrix;
  mdl.NumObservations = size(X, 1);
  mdl.NumPredictors = size(X, 2);
  mdl.LearnerOptions = rmfield(opts, 'ClassNames');
end
%=============================================================================
function opts = parseOptionsLocal(args, X, classNames)
  opts = struct('ClassNames', {classNames}, 'KernelFunction', 'linear', ...
    'KernelScale', sqrt(size(X, 2)), 'PolynomialOrder', 3, 'BoxConstraint', 1, ...
    'Standardize', false, 'IterationLimit', 1000, 'Tolerance', 1e-3);
  for k = 1:2:length(args)
    name = __option_name__(args{k}, {'classnames', 'kernelfunction', 'kernelscale', ...
      'polynomialorder', 'boxconstraint', 'standardize', 'iterationlimit', 'tolerance'});
    value = args{k + 1};
    switch name
      case 'classnames'
        opts.ClassNames = __class_names__(value, classNames);
      case 'kernelfunction'
        opts.KernelFunction = kernelFunctionLocal(value);
      case 'kernelscale'
        opts.KernelScale = kernelScaleLocal(value, X);
      case 'polynomialorder'
        opts.PolynomialOrder = positiveIntegerLocal(value, 'PolynomialOrder');
      case 'boxconstraint'
        opts.BoxConstraint = positiveScalarLocal(value, 'BoxConstraint');
      case 'standardize'
        opts.Standardize = logicalScalarLocal(value, 'Standardize');
      case 'iterationlimit'
        opts.IterationLimit = positiveIntegerLocal(value, 'IterationLimit');
      case 'tolerance'
        opts.Tolerance = positiveScalarLocal(value, 'Tolerance');
    end
  end
  if ~isequal(opts.ClassNames, classNames)
    __check_class_names__(classNames, opts.ClassNames, true);
  end
end
%=============================================================================
function learners = trainLearnersLocal(X, yIndex, classNames, Yclean, opts)
  k = length(classNames);
  nLearners = k * (k - 1) / 2;
  binaryLearners = cell(1, nLearners);
  coding = zeros(k, nLearners);
  m = 0;
  for i = 1:k
    for j = i + 1:k
      m = m + 1;
      rows = yIndex == i | yIndex == j;
      pairNames = __labels_from_class_index__(classNames, [i; j], 2);
      binaryLearners{m} = fitcsvm(X(rows, :), Yclean(rows), ...
        'ClassNames', pairNames, 'KernelFunction', opts.KernelFunction, ...
        'KernelScale', opts.KernelScale, 'PolynomialOrder', opts.PolynomialOrder, ...
        'BoxConstraint', opts.BoxConstraint, 'Standardize', opts.Standardize, ...
        'IterationLimit', opts.IterationLimit, 'Tolerance', opts.Tolerance);
      coding(i, m) = -1;
      coding(j, m) = 1;
    end
  end
  learners = struct('BinaryLearners', {binaryLearners}, 'CodingMatrix', coding);
end
%=============================================================================
function name = kernelFunctionLocal(value)
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'KernelFunction');
  end
  text = lower(char(value));
  names = {'linear', 'gaussian', 'rbf', 'polynomial'};
  exact = names(strcmp(names, text));
  if length(exact) == 1
    name = exact{1};
  else
    matches = names(startsWith(names, text));
    if length(matches) ~= 1
      error('Nelson:statistics:invalidKernelFunctionValue', 'Invalid %s value.', 'KernelFunction');
    end
    name = matches{1};
  end
  if strcmp(name, 'rbf')
    name = 'gaussian';
  end
end
%=============================================================================
function value = kernelScaleLocal(value, X)
  if ischar(value) || (isstring(value) && isscalar(value))
    if strcmpi(char(value), 'auto')
      value = sqrt(size(X, 2));
      return
    end
  end
  value = positiveScalarLocal(value, 'KernelScale');
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
function value = positiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositive', '%s must be a positive scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = logicalScalarLocal(value, name)
  if islogical(value) && isscalar(value)
    return
  end
  if isnumeric(value) && isscalar(value) && (value == 0 || value == 1)
    value = logical(value);
    return
  end
  error('Nelson:statistics:mustBeTrueOrFalse', '%s must be true or false.', name);
end
%=============================================================================
