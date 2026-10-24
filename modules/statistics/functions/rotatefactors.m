%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, T] = rotatefactors(X, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 2);
  preserveSingle = isa(X, 'single');
  X = validateLoadingsLocal(X);
  opts = parseOptionsLocal(convertStringToCharArgs(varargin), size(X));
  [B, T] = rotateLocal(X, opts);
  if preserveSingle
    B = single(B);
    T = single(T);
  end
end
%=============================================================================
function X = validateLoadingsLocal(X)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  X = double(X);
  if isempty(X) || any(~isfinite(X(:)))
    error('Nelson:statistics:xMustBeFinite', '%s must contain finite values.', 'X');
  end
  if size(X, 2) < 1
    error('Nelson:statistics:xContainLeastOneFactor', 'X must contain at least one factor.');
  end
end
%=============================================================================
function opts = parseOptionsLocal(args, loadingsSize)
  opts = defaultOptionsLocal();
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    opts = setOptionLocal(opts, lower(char(args{k})), args{k + 1}, loadingsSize);
  end
end
%=============================================================================
function opts = defaultOptionsLocal()
  opts = struct('Method', 'varimax', 'Normalize', true, 'RelTol', sqrt(eps), ...
    'MaxIt', 250, 'Coeff', 1, 'Power', 4, 'Target', [], 'Type', 'oblique');
end
%=============================================================================
function opts = setOptionLocal(opts, name, value, loadingsSize)
  switch name
    case 'method'
      opts.Method = parseChoiceLocal(value, {'varimax', 'quartimax', 'equamax', ...
        'parsimax', 'orthomax', 'promax', 'procrustes', 'pattern'}, 'Method');
    case 'normalize'
      opts.Normalize = parseOnOffLocal(value, 'Normalize');
    case 'reltol'
      opts.RelTol = parsePositiveScalarLocal(value, 'RelTol');
    case 'maxit'
      opts.MaxIt = parsePositiveIntegerLocal(value, 'MaxIt');
    case 'coeff'
      opts.Coeff = parseNonnegativeScalarLocal(value, 'Coeff');
    case 'power'
      opts.Power = parsePositiveScalarLocal(value, 'Power');
    case 'target'
      opts.Target = validateTargetLocal(value, loadingsSize);
    case 'type'
      opts.Type = parseChoiceLocal(value, {'oblique', 'orthogonal'}, 'Type');
    otherwise
      error('Nelson:statistics:unknownOption', 'Unknown option.');
  end
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  value = lower(char(value));
  matches = choices(startsWith(choices, value));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = matches{1};
end
%=============================================================================
function tf = parseOnOffLocal(value, name)
  if islogical(value) && isscalar(value)
    tf = value ~= 0;
  elseif isnumeric(value) && isreal(value) && isscalar(value) && (value == 0 || value == 1)
    tf = value ~= 0;
  elseif isTextScalarLocal(value)
    value = lower(char(value));
    if strcmp(value, 'on')
      tf = true;
    elseif strcmp(value, 'off')
      tf = false;
    else
      error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
    end
  else
    error('Nelson:statistics:invalidValue:valueOffTrueFalse', '%s must be on, off, true, or false.', name);
  end
end
%=============================================================================
function value = parsePositiveScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value > 0)
    error('Nelson:statistics:mustBePositiveFinite', '%s must be a positive finite scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = parseNonnegativeScalarLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) && value >= 0)
    error('Nelson:statistics:mustBeNonnegativeFinite', '%s must be a nonnegative finite scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function value = parsePositiveIntegerLocal(value, name)
  if ~(isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
      && floor(value) == value && value > 0)
    error('Nelson:statistics:mustBePositiveInteger', '%s must be a positive integer scalar.', name);
  end
  value = double(value);
end
%=============================================================================
function target = validateTargetLocal(target, loadingsSize)
  if ~(isnumeric(target) && isreal(target) && ~issparse(target) && ndims(target) == 2)
    error('Nelson:statistics:targetRealNumericMatrix', '%s must be a real numeric matrix.', 'Target');
  end
  target = double(target);
  if ~isequal(size(target), loadingsSize)
    error('Nelson:statistics:targetSameSizeX', 'Target must have the same size as X.');
  end
  if any(~isfinite(target(:)))
    error('Nelson:statistics:targetContainFiniteValues', '%s must contain finite values.', 'Target');
  end
end
%=============================================================================
function [B, T] = rotateLocal(X, opts)
  switch opts.Method
    case {'varimax', 'quartimax', 'equamax', 'parsimax', 'orthomax'}
      [B, T] = orthomaxLocal(X, opts, rotationCoeffLocal(opts.Method, opts.Coeff));
    case 'promax'
      [B, T] = promaxLocal(X, opts);
    otherwise
      [B, T] = targetRotationLocal(X, opts);
  end
  [B, T] = orientColumnsLocal(B, T);
end
%=============================================================================
function gamma = rotationCoeffLocal(method, coeff)
  switch method
    case 'quartimax'
      gamma = 0;
    case {'equamax', 'parsimax'}
      gamma = [];
    case 'orthomax'
      gamma = coeff;
    otherwise
      gamma = 1;
  end
end
%=============================================================================
function [B, T] = orthomaxLocal(X, opts, gamma)
  [Xn, scale] = normalizeRowsLocal(X, opts.Normalize);
  [p, m] = size(Xn);
  if isempty(gamma)
    if strcmp(opts.Method, 'equamax')
      gamma = m / 2;
    else
      gamma = p * (m - 1) / max(p + m - 2, 1);
    end
  end
  T = eye(m);
  previous = 0;
  for iter = 1:opts.MaxIt
    rotated = Xn * T;
    target = p * rotated .^ 3 - gamma * (rotated * diag(sum(rotated .^ 2, 1)));
    [U, S, V] = svd(Xn' * target);
    T = U * V';
    current = sum(diag(S));
    if current <= previous * (1 + opts.RelTol)
      break
    end
    previous = current;
  end
  B = unnormalizeRowsLocal(Xn * T, scale);
end
%=============================================================================
function [B, T] = promaxLocal(X, opts)
  [orthogonal, Tortho] = orthomaxLocal(X, opts, opts.Coeff);
  target = sign(orthogonal) .* abs(orthogonal) .^ opts.Power;
  procrustesOpts = opts;
  procrustesOpts.Target = target;
  procrustesOpts.Type = 'oblique';
  [B, Tp] = targetRotationLocal(X, procrustesOpts);
  T = Tortho * (Tortho \ Tp);
end
%=============================================================================
function [B, T] = targetRotationLocal(X, opts)
  if isempty(opts.Target)
    error('Nelson:statistics:targetRequiredRotationMethod', 'Target is required for this rotation method.');
  end
  if strcmp(opts.Type, 'orthogonal')
    [U, ~, V] = svd(X' * opts.Target);
    T = U * V';
  else
    T = (X' * X) \ (X' * opts.Target);
    columnNorm = sqrt(sum(T .^ 2, 1));
    columnNorm(columnNorm == 0) = 1;
    T = T ./ repmat(columnNorm, size(T, 1), 1);
  end
  B = X * T;
end
%=============================================================================
function [Xn, scale] = normalizeRowsLocal(X, doNormalize)
  scale = ones(size(X, 1), 1);
  Xn = X;
  if ~doNormalize
    return
  end
  scale = sqrt(sum(X .^ 2, 2));
  scale(scale <= 0) = 1;
  Xn = X ./ repmat(scale, 1, size(X, 2));
end
%=============================================================================
function X = unnormalizeRowsLocal(X, scale)
  X = X .* repmat(scale, 1, size(X, 2));
end
%=============================================================================
function [B, T] = orientColumnsLocal(B, T)
  for k = 1:size(B, 2)
    [~, idx] = max(abs(B(:, k)));
    if B(idx, k) < 0
      B(:, k) = -B(:, k);
      T(:, k) = -T(:, k);
    end
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
