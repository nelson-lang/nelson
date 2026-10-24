%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rho, pval] = partialcorr(x, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 2);
  [y, z, mode, opts] = parseInputsLocal(x, convertStringToCharArgs(varargin));
  [x, y, z] = validateInputsLocal(x, y, z);
  if strcmp(opts.Type, 'spearman')
    x = rankColumnsLocal(x);
    y = rankColumnsLocal(y);
    z = rankColumnsLocal(z);
  end
  if strcmp(opts.Rows, 'complete')
    [x, y, z] = completeRowsLocal(x, y, z);
  end
  switch mode
    case 'self'
      [rho, n, controls] = partialSelfLocal(x, opts.Rows);
    case 'controlled'
      [rho, n, controls] = partialMatrixLocal(x, x, z, opts.Rows);
    otherwise
      [rho, n, controls] = partialMatrixLocal(x, y, z, opts.Rows);
  end
  pval = pvalueLocal(rho, n, controls, opts.Tail);
end
%=============================================================================
function [y, z, mode, opts] = parseInputsLocal(x, args)
  opts = struct('Type', 'pearson', 'Rows', 'all', 'Tail', 'both');
  y = [];
  z = [];
  mode = 'self';
  dataArgs = {};
  while ~isempty(args) && ~isTextScalarLocal(args{1}) && length(dataArgs) < 2
    dataArgs{end + 1} = args{1};
    args = args(2:end);
  end
  if length(dataArgs) == 1
    z = dataArgs{1};
    mode = 'controlled';
  elseif length(dataArgs) == 2
    y = dataArgs{1};
    z = dataArgs{2};
    mode = 'cross';
  end
  if isempty(y)
    y = x;
  end
  if isempty(z)
    z = zeros(size(x, 1), 0);
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case 'type'
        opts.Type = parseChoiceLocal(args{k + 1}, {'pearson', 'spearman'}, 'TYPE');
      case 'rows'
        opts.Rows = parseChoiceLocal(args{k + 1}, {'all', 'complete', 'pairwise'}, 'ROWS');
      case 'tail'
        opts.Tail = parseChoiceLocal(args{k + 1}, {'both', 'right', 'left'}, 'TAIL');
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function [x, y, z] = validateInputsLocal(x, y, z)
  if ~(isnumeric(x) && isreal(x) && ~issparse(x) && ismatrix(x))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(y) && isreal(y) && ~issparse(y) && ismatrix(y))
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  if ~(isnumeric(z) && isreal(z) && ~issparse(z) && ismatrix(z))
    error('Nelson:statistics:zRealNumericMatrix', '%s must be a real numeric matrix.', 'Z');
  end
  if size(x, 1) ~= size(y, 1) || size(x, 1) ~= size(z, 1)
    error('Nelson:statistics:xYZSameNumber', '%s must have the same number of rows.', 'X, Y, and Z');
  end
  x = double(x);
  y = double(y);
  z = double(z);
end
%=============================================================================
function [x, y, z] = completeRowsLocal(x, y, z)
  keep = all(~isnan(x), 2) & all(~isnan(y), 2) & all(~isnan(z), 2);
  x = x(keep, :);
  y = y(keep, :);
  z = z(keep, :);
end
%=============================================================================
function R = rankColumnsLocal(X)
  R = NaN(size(X));
  for k = 1:size(X, 2)
    R(:, k) = tiedrank(X(:, k));
  end
end
%=============================================================================
function [rho, n, controls] = partialSelfLocal(x, rowsMode)
  p = size(x, 2);
  rho = eye(p);
  n = zeros(p, p);
  controls = zeros(p, p);
  for i = 1:p
    for j = i + 1:p
      controlColumns = setdiff(1:p, [i j], 'stable');
      [r, count, k] = partialPairLocal(x(:, i), x(:, j), x(:, controlColumns), rowsMode);
      rho(i, j) = r;
      rho(j, i) = r;
      n(i, j) = count;
      n(j, i) = count;
      controls(i, j) = k;
      controls(j, i) = k;
    end
    n(i, i) = size(x, 1);
    controls(i, i) = max(0, p - 1);
  end
end
%=============================================================================
function [rho, n, controls] = partialMatrixLocal(x, y, z, rowsMode)
  px = size(x, 2);
  py = size(y, 2);
  rho = NaN(px, py);
  n = zeros(px, py);
  controls = size(z, 2) * ones(px, py);
  for i = 1:px
    for j = 1:py
      [rho(i, j), n(i, j), controls(i, j)] = partialPairLocal(x(:, i), y(:, j), z, rowsMode);
    end
  end
end
%=============================================================================
function [r, n, k] = partialPairLocal(x, y, z, rowsMode)
  keep = rowsToUseLocal(x, y, z, rowsMode);
  x = x(keep);
  y = y(keep);
  z = z(keep, :);
  n = length(x);
  k = size(z, 2);
  if n <= k + 1 || any(isnan(x)) || any(isnan(y)) || any(isnan(z(:)))
    r = NaN;
    return
  end
  rx = residualLocal(x, z);
  ry = residualLocal(y, z);
  r = corr(rx, ry);
  r = clampCorrelationLocal(r);
end
%=============================================================================
function keep = rowsToUseLocal(x, y, z, rowsMode)
  switch rowsMode
    case 'all'
      keep = true(size(x));
    otherwise
      keep = ~isnan(x) & ~isnan(y) & all(~isnan(z), 2);
  end
end
%=============================================================================
function r = residualLocal(x, z)
  if isempty(z)
    r = x;
    return
  end
  design = [ones(size(z, 1), 1), z];
  r = x - design * (design \ x);
end
%=============================================================================
function r = clampCorrelationLocal(r)
  tol = 10 * eps;
  if r > 1 && r <= 1 + tol
    r = 1;
  elseif r < -1 && r >= -1 - tol
    r = -1;
  end
end
%=============================================================================
function p = pvalueLocal(r, n, controls, tail)
  p = NaN(size(r));
  df = n - controls - 2;
  valid = ~isnan(r) & df > 0 & abs(r) < 1;
  t = r(valid) .* sqrt(df(valid) ./ max(1 - r(valid) .^ 2, 0));
  p(valid) = tailProbabilityFromTLocal(t, df(valid), tail);
  perfect = ~isnan(r) & df > 0 & abs(r) == 1;
  if any(perfect(:))
    p(perfect) = perfectCorrelationPValueLocal(r(perfect), tail);
  end
end
%=============================================================================
function p = tailProbabilityFromTLocal(t, v, tail)
  switch tail
    case 'right'
      p = 1 - tcdf(t, v);
    case 'left'
      p = tcdf(t, v);
    otherwise
      p = 2 * min(tcdf(t, v), 1 - tcdf(t, v));
  end
end
%=============================================================================
function p = perfectCorrelationPValueLocal(r, tail)
  p = zeros(size(r));
  switch tail
    case 'right'
      p(r < 0) = 1;
    case 'left'
      p(r > 0) = 1;
  end
end
%=============================================================================
function value = parseChoiceLocal(value, choices, name)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
  text = lower(char(value));
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:invalidValue:valueHasUnsupportedValue', '%s has an unsupported value.', name);
  end
  value = matches{1};
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
