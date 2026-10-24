%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [W, H, D] = nnmf(A, k, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 3);
  preserveSingle = isa(A, 'single');
  [A, k, opts] = parseInputsLocal(A, k, convertStringToCharArgs(varargin));
  [W, H, D] = factorizeLocal(A, k, opts);
  [W, H] = normalizeFactorsLocal(W, H);
  D = rmsResidualLocal(A, W, H);
  if preserveSingle
    W = single(W);
    H = single(H);
    D = single(D);
  end
end
%=============================================================================
function [A, k, opts] = parseInputsLocal(A, k, args)
  if ~(isnumeric(A) && isreal(A) && ~issparse(A) && ndims(A) == 2)
    error('Nelson:statistics:realNumericMatrix', '%s must be a real numeric matrix.', 'A');
  end
  A = double(A);
  if isempty(A) || any(~isfinite(A(:))) || any(A(:) < 0)
    error('Nelson:statistics:mustBeFiniteNonnegative', '%s must contain finite nonnegative values.', 'A');
  end
  if ~(isnumeric(k) && isreal(k) && isscalar(k) && isfinite(k) ...
      && floor(k) == k && k > 0)
    error('Nelson:statistics:kPositiveIntegerScalar', '%s must be a positive integer scalar.', 'k');
  end
  k = double(k);
  opts = defaultOptionsLocal();
  opts = parseNameValuesLocal(opts, args);
  opts.W0 = validateInitialLocal(opts.W0, [size(A, 1), k], 'W0');
  opts.H0 = validateInitialLocal(opts.H0, [k, size(A, 2)], 'H0');
  opts.Replicates = parseReplicatesLocal(opts.Replicates);
  opts.Options = parseOptionsLocal(opts.Options, opts.Algorithm);
end
%=============================================================================
function opts = defaultOptionsLocal()
  opts = struct('Algorithm', 'als', 'W0', [], 'H0', [], ...
    'Options', [], 'Replicates', 1);
end
%=============================================================================
function opts = parseNameValuesLocal(opts, args)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for i = 1:2:length(args)
    if ~isTextScalarLocal(args{i})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{i}));
    switch name
      case 'algorithm'
        opts.Algorithm = parseAlgorithmLocal(args{i + 1});
      case 'w0'
        opts.W0 = args{i + 1};
      case 'h0'
        opts.H0 = args{i + 1};
      case 'options'
        opts.Options = args{i + 1};
      case 'replicates'
        opts.Replicates = args{i + 1};
      otherwise
        error('Nelson:statistics:unknownOption', 'Unknown option.');
    end
  end
end
%=============================================================================
function value = parseAlgorithmLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Algorithm');
  end
  value = lower(char(value));
  choices = {'als', 'mult'};
  matches = choices(strncmp(choices, value, length(value)));
  if isempty(matches)
    error('Nelson:statistics:invalidAlgorithmValue', 'Invalid %s value.', 'Algorithm');
  end
  if length(matches) > 1
    error('Nelson:statistics:ambiguousValue', 'Ambiguous %s value.', 'Algorithm');
  end
  value = matches{1};
end
%=============================================================================
function X0 = validateInitialLocal(X0, expectedSize, name)
  if isempty(X0)
    return
  end
  if ~(isnumeric(X0) && isreal(X0) && ~issparse(X0) && ndims(X0) == 2)
    error('Nelson:statistics:realNumericMatrix', '%s must be a real numeric matrix.', name);
  end
  X0 = double(X0);
  if ~isequal(size(X0), expectedSize)
    error('Nelson:statistics:sizeMismatch', '%s has incompatible size.', name);
  end
  if any(~isfinite(X0(:))) || any(X0(:) < 0)
    error('Nelson:statistics:mustBeFiniteNonnegative', '%s must contain finite nonnegative values.', name);
  end
end
%=============================================================================
function replicates = parseReplicatesLocal(replicates)
  if ~(isnumeric(replicates) && isreal(replicates) && isscalar(replicates) ...
      && isfinite(replicates) && floor(replicates) == replicates && replicates > 0)
    error('Nelson:statistics:replicatesPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Replicates');
  end
  replicates = double(replicates);
end
%=============================================================================
function options = parseOptionsLocal(options, algorithm)
  if isempty(options)
    options = statset();
  elseif ~(isstruct(options) && isscalar(options))
    error('Nelson:statistics:invalidOptions', '%s must be a scalar structure.', 'Options');
  end
  displayValue = statget(options, 'Display', 'off');
  if ~isTextScalarLocal(displayValue)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Options Display');
  end
  options.Display = lower(char(displayValue));
  defaultMaxIter = 50;
  if strcmp(algorithm, 'mult')
    defaultMaxIter = 100;
  end
  options.MaxIter = statget(options, 'MaxIter', defaultMaxIter);
  options.TolFun = statget(options, 'TolFun', 1e-4);
  options.TolX = statget(options, 'TolX', 1e-4);
  if ~any(strcmp(options.Display, {'off', 'final', 'iter'}))
    error('Nelson:statistics:optionsDisplayInvalidValue', '%s has an invalid value.', 'Options Display');
  end
  if ~isPositiveIntegerLocal(options.MaxIter)
    error('Nelson:statistics:optionsMaxIterPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Options MaxIter');
  end
  if ~isNonnegativeScalarLocal(options.TolFun) || ~isNonnegativeScalarLocal(options.TolX)
    error('Nelson:statistics:optionsTolerancesNonnegativeScalarValues', 'Options tolerances must be %s scalar values.', 'nonnegative');
  end
end
%=============================================================================
function [bestW, bestH, bestD] = factorizeLocal(A, k, opts)
  bestD = Inf;
  bestW = [];
  bestH = [];
  for rep = 1:opts.Replicates
    [W, H] = startFactorsLocal(A, k, opts, rep);
    [W, H, D, iterations, step] = iterateLocal(A, W, H, opts);
    if D < bestD
      bestD = D;
      bestW = W;
      bestH = H;
    end
    displayProgressLocal(opts.Options.Display, 'final', rep, iterations, D, step);
  end
end
%=============================================================================
function [W, H] = startFactorsLocal(A, k, opts, rep)
  useInitial = rep == 1 && (~isempty(opts.W0) || ~isempty(opts.H0));
  if useInitial && ~isempty(opts.W0)
    W = opts.W0;
  else
    W = max(rand(size(A, 1), k), eps);
  end
  if useInitial && ~isempty(opts.H0)
    H = opts.H0;
  else
    H = max(rand(k, size(A, 2)), eps);
  end
end
%=============================================================================
function [W, H, D, iterations, step] = iterateLocal(A, W, H, opts)
  D = rmsResidualLocal(A, W, H);
  step = Inf;
  for iterations = 1:opts.Options.MaxIter
    oldW = W;
    oldH = H;
    if strcmp(opts.Algorithm, 'mult')
      [W, H] = multStepLocal(A, W, H);
    else
      [W, H] = alsStepLocal(A, W, H);
    end
    Dnew = rmsResidualLocal(A, W, H);
    step = factorStepLocal(W, H, oldW, oldH);
    displayProgressLocal(opts.Options.Display, 'iter', 1, iterations, Dnew, step);
    if abs(D - Dnew) <= opts.Options.TolFun * max(1, D) || step <= opts.Options.TolX
      D = Dnew;
      break
    end
    D = Dnew;
  end
end
%=============================================================================
function [W, H] = multStepLocal(A, W, H)
  tiny = sqrt(realmin);
  H = H .* ((W' * A) ./ max(W' * W * H, tiny));
  W = W .* ((A * H') ./ max(W * (H * H'), tiny));
  W = max(W, tiny);
  H = max(H, tiny);
end
%=============================================================================
function [W, H] = alsStepLocal(A, W, H)
  tiny = sqrt(realmin);
  H = max((W' * W + tiny * eye(size(W, 2))) \ (W' * A), 0);
  W = max((A * H') / (H * H' + tiny * eye(size(H, 1))), 0);
  W = max(W, tiny);
  H = max(H, tiny);
end
%=============================================================================
function step = factorStepLocal(W, H, oldW, oldH)
  numerator = norm(W - oldW, 'fro') + norm(H - oldH, 'fro');
  denominator = max(1, norm(oldW, 'fro') + norm(oldH, 'fro'));
  step = numerator / denominator;
end
%=============================================================================
function D = rmsResidualLocal(A, W, H)
  D = norm(A - W * H, 'fro') / sqrt(numel(A));
end
%=============================================================================
function [W, H] = normalizeFactorsLocal(W, H)
  scale = sqrt(sum(H .^ 2, 2));
  scale(scale <= 0) = 1;
  W = W .* repmat(scale(:)', size(W, 1), 1);
  H = H ./ repmat(scale(:), 1, size(H, 2));
  columnLength = sqrt(sum(W .^ 2, 1));
  [~, order] = sort(columnLength, 'descend');
  W = W(:, order);
  H = H(order, :);
end
%=============================================================================
function displayProgressLocal(displayValue, stage, rep, iterations, D, step)
  if strcmp(displayValue, 'iter') && strcmp(stage, 'iter')
    disp([int2str(iterations), ' iterations, rms resid = ', num2str(D), ...
      ' |delta x| = ', num2str(step)]);
  elseif strcmp(displayValue, 'final') && strcmp(stage, 'final')
    disp(['rep ', int2str(rep), ', ', int2str(iterations), ...
      ' iterations, Final root mean square residual = ', num2str(D)]);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
function tf = isPositiveIntegerLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) ...
    && isfinite(value) && floor(value) == value && value > 0;
end
%=============================================================================
function tf = isNonnegativeScalarLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value >= 0;
end
%=============================================================================
