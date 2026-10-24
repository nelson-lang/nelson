%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, TF, L, U, C] = filloutliers(A, fillmethod, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 5);
  if ~(isnumeric(A) && isreal(A) && ~issparse(A))
    error('Nelson:statistics:aMustBeRealArray', '%s must be a real numeric array.', 'A');
  end
  [opts, fill] = parseInputsLocal(fillmethod, convertStringToCharArgs(varargin));
  [TF, L, U, C] = detectOutliersLocal(A, opts);
  B = fillOutliersAlongDimensionLocal(A, TF, L, U, C, fill, opts);
end
%=============================================================================
function [opts, fill] = parseInputsLocal(fillmethod, args)
  opts = struct('DetectArgs', {{}}, 'Dim', [], 'NameValueArgs', {{}}, 'OutlierLocations', []);
  fill = parseFillMethodLocal(fillmethod);
  idx = 1;
  if idx <= length(args)
    value = args{idx};
    if isTextScalarLocal(value) && ~isLeadingNameValueLocal(value)
      method = parseDetectionMethodLocal(value);
      opts.DetectArgs{end + 1} = value;
      idx = idx + 1;
      if any(strcmp(method, {'percentiles', 'movmedian', 'movmean'}))
        if idx > length(args)
          error('Nelson:statistics:detectionMethodArgumentIncomplete', 'Detection method argument is incomplete.');
        end
        opts.DetectArgs{end + 1} = args{idx};
        idx = idx + 1;
      end
    elseif isDimScalarLocal(value)
      opts.Dim = double(value);
      idx = idx + 1;
    end
  end
  if idx <= length(args) && isDimScalarLocal(args{idx})
    opts.Dim = double(args{idx});
    idx = idx + 1;
  end
  [opts, fill] = parseNameValuesLocal(opts, fill, args(idx:end));
end
%=============================================================================
function [opts, fill] = parseNameValuesLocal(opts, fill, args)
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    name = matchedNameLocal(args{k});
    switch name
      case 'outlierlocations'
        opts.OutlierLocations = args{k + 1};
      case 'samplepoints'
        opts.NameValueArgs{end + 1} = args{k};
        opts.NameValueArgs{end + 1} = args{k + 1};
        fill.SamplePoints = parseSamplePointsLocal(args{k + 1});
      case {'thresholdfactor', 'maxnumoutliers'}
        opts.NameValueArgs{end + 1} = args{k};
        opts.NameValueArgs{end + 1} = args{k + 1};
      case {'datavariables', 'outputformat'}
        error('Nelson:statistics:invalidUsage', '%s is not supported for numeric array input.', canonicalNameLocal(name));
    end
  end
end
%=============================================================================
function [TF, L, U, C] = detectOutliersLocal(A, opts)
  if isempty(opts.Dim)
    dim = firstNonsingletonLocal(size(A));
  else
    dim = opts.Dim;
  end
  if isempty(opts.OutlierLocations)
    [TF, L, U, C] = isoutlier(A, opts.DetectArgs{:}, dim, opts.NameValueArgs{:});
  else
    TF = parseOutlierLocationsLocal(opts.OutlierLocations, size(A));
    [L, U, C] = emptyThresholdsLocal(size(A), dim);
  end
end
%=============================================================================
function B = fillOutliersAlongDimensionLocal(A, TF, L, U, C, fill, opts)
  if isempty(opts.Dim)
    dim = firstNonsingletonLocal(size(A));
  else
    dim = opts.Dim;
  end
  sz = size(A);
  originalRank = max(2, length(sz));
  nd = max(originalRank, dim);
  sz(end + 1:nd) = 1;
  fill = validateSamplePointsLocal(fill, sz(dim));
  perm = [dim, setdiff(1:nd, dim, 'stable')];
  Ap = permute(reshape(A, sz), perm);
  TFp = permute(reshape(TF, sz), perm);
  psize = size(Ap);
  psize(end + 1:nd) = 1;
  n = psize(1);
  cols = prod(psize(2:end));
  Br = reshape(Ap, n, cols);
  TFr = reshape(TFp, n, cols);
  thresholds = thresholdsToRowsLocal(L, U, C, fill, perm, psize, sz, dim);
  for col = 1:cols
    Br(:, col) = fillVectorLocal(Br(:, col), TFr(:, col), thresholds, col, fill);
  end
  B = reshape(ipermute(reshape(Br, psize), perm), sz(1:originalRank));
end
%=============================================================================
function thresholds = thresholdsToRowsLocal(L, U, C, fill, perm, psize, sz, dim)
  thresholds = struct('L', [], 'U', [], 'C', []);
  if strcmp(fill.Method, 'clip')
    if isequal(size(L), sz)
      thresholds.L = reshape(permute(reshape(L, sz), perm), psize);
      thresholds.U = reshape(permute(reshape(U, sz), perm), psize);
      thresholds.C = reshape(permute(reshape(C, sz), perm), psize);
      thresholds.L = reshape(thresholds.L, psize(1), []);
      thresholds.U = reshape(thresholds.U, psize(1), []);
      thresholds.C = reshape(thresholds.C, psize(1), []);
    else
      thresholds.L = reshapeThresholdLocal(L, perm, psize, sz, dim);
      thresholds.U = reshapeThresholdLocal(U, perm, psize, sz, dim);
      thresholds.C = reshapeThresholdLocal(C, perm, psize, sz, dim);
    end
  end
end
%=============================================================================
function values = reshapeThresholdLocal(T, perm, psize, sz, dim)
  outSize = sz;
  outSize(dim) = 1;
  Tp = permute(reshape(T, outSize), perm);
  values = reshape(Tp, 1, prod(psize(2:end)));
end
%=============================================================================
function y = fillVectorLocal(x, mask, thresholds, col, fill)
  y = x;
  if ~any(mask)
    return
  end
  switch fill.Method
    case 'constant'
      y(mask) = castConstantLocal(fill.Constant, x);
    case 'clip'
      y = fillClipLocal(y, mask, thresholds, col);
    case 'previous'
      y = fillPreviousLocal(y, mask);
    case 'next'
      y = fillNextLocal(y, mask);
    case 'nearest'
      y = fillNearestLocal(y, mask);
    otherwise
      y = fillInterpLocal(y, mask, fill);
  end
end
%=============================================================================
function y = fillClipLocal(y, mask, thresholds, col)
  idx = find(mask);
  for k = idx(:)'
    if size(thresholds.L, 1) == 1
      lo = thresholds.L(1, col);
      hi = thresholds.U(1, col);
    else
      lo = thresholds.L(k, col);
      hi = thresholds.U(k, col);
    end
    y(k) = min(max(y(k), lo), hi);
  end
end
%=============================================================================
function y = fillPreviousLocal(y, mask)
  for k = 1:length(y)
    if mask(k)
      previous = find(~mask(1:k - 1), 1, 'last');
      if ~isempty(previous)
        y(k) = y(previous);
      end
    end
  end
end
%=============================================================================
function y = fillNextLocal(y, mask)
  for k = length(y):-1:1
    if mask(k)
      next = find(~mask(k + 1:end), 1, 'first');
      if ~isempty(next)
        y(k) = y(k + next);
      end
    end
  end
end
%=============================================================================
function y = fillNearestLocal(y, mask)
  known = find(~mask);
  for k = find(mask(:)).'
    if isempty(known)
      continue
    end
    [~, pos] = min(abs(known - k));
    y(k) = y(known(pos));
  end
end
%=============================================================================
function y = fillInterpLocal(y, mask, fill)
  known = find(~mask);
  missing = find(mask);
  if length(known) < 2
    return
  end
  x = samplePointsForLengthLocal(fill, length(y));
  yi = interp1(x(known), double(y(known)), x(missing), fill.Method);
  finiteValues = ~isnan(yi);
  y(missing(finiteValues)) = cast(yi(finiteValues), class(y));
end
%=============================================================================
function fill = parseFillMethodLocal(value)
  fill = struct('Method', '', 'Constant', [], 'SamplePoints', []);
  if isTextScalarLocal(value)
    choices = {'previous', 'next', 'nearest', 'linear', 'spline', 'pchip', 'makima', 'clip'};
    text = lower(char(value));
    matches = choices(startsWith(choices, text));
    if length(matches) ~= 1
      error('Nelson:statistics:FILLMETHODUnsupportedValue', '%s has an unsupported value.', 'FILLMETHOD');
    end
    fill.Method = matches{1};
    if any(strcmp(fill.Method, {'spline', 'makima'}))
      error('Nelson:statistics:unsupportedFillMethod', 'Unsupported %s.', 'fill method');
    end
  elseif isnumeric(value) && isreal(value) && isscalar(value)
    fill.Method = 'constant';
    fill.Constant = value;
  else
    error('Nelson:statistics:FILLMETHODSupportedMethodNameNumeric', 'FILLMETHOD must be a supported method name or numeric scalar.');
  end
end
%=============================================================================
function method = parseDetectionMethodLocal(value)
  choices = {'median', 'mean', 'quartiles', 'grubbs', 'gesd', ...
    'percentiles', 'movmedian', 'movmean'};
  text = lower(char(value));
  matches = choices(startsWith(choices, text));
  if length(matches) ~= 1
    error('Nelson:statistics:METHODUnsupportedValue', '%s has an unsupported value.', 'METHOD');
  end
  method = matches{1};
end
%=============================================================================
function value = castConstantLocal(value, target)
  value = cast(value, class(target));
end
%=============================================================================
function x = samplePointsForLengthLocal(fill, n)
  if isempty(fill.SamplePoints)
    x = (1:n)';
  else
    x = fill.SamplePoints(:);
  end
end
%=============================================================================
function fill = validateSamplePointsLocal(fill, n)
  if isempty(fill.SamplePoints)
    return
  end
  if length(fill.SamplePoints) ~= n
    error('Nelson:statistics:samplePointsOneElementInputElement', 'SamplePoints must have one element for each input element along the operating dimension.');
  end
  if any(diff(fill.SamplePoints) <= 0)
    error('Nelson:statistics:samplePointsSortedContainUniqueValues', 'SamplePoints must be sorted and contain unique values.');
  end
end
%=============================================================================
function samplePoints = parseSamplePointsLocal(value)
  if ~(isnumeric(value) && isreal(value) && isvector(value) && all(isfinite(value)))
    error('Nelson:statistics:samplePointsFiniteRealNumericVector', 'SamplePoints must be a finite real numeric vector.');
  end
  samplePoints = double(value(:));
end
%=============================================================================
function locations = parseOutlierLocationsLocal(value, sz)
  if ~(islogical(value) && isequal(size(value), sz))
    error('Nelson:statistics:outlierLocationsLogicalArraySameSize', 'OutlierLocations must be a logical array the same size as A.');
  end
  locations = value;
end
%=============================================================================
function [L, U, C] = emptyThresholdsLocal(sz, dim)
  outSize = sz;
  outSize(end + 1:dim) = 1;
  outSize(dim) = 1;
  L = NaN(outSize);
  U = NaN(outSize);
  C = NaN(outSize);
end
%=============================================================================
function dim = firstNonsingletonLocal(sz)
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
function tf = isDimScalarLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
    && value == fix(value) && value >= 1;
end
%=============================================================================
function name = matchedNameLocal(value)
  if ~isTextScalarLocal(value)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
  end
  names = {'thresholdfactor', 'maxnumoutliers', 'samplepoints', 'outlierlocations', ...
    'datavariables', 'outputformat'};
  text = lower(char(value));
  matches = names(startsWith(names, text));
  if length(matches) ~= 1
    error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
  end
  name = matches{1};
end
%=============================================================================
function tf = isLeadingNameValueLocal(value)
  names = {'thresholdfactor', 'maxnumoutliers', 'samplepoints', 'outlierlocations', ...
    'datavariables', 'outputformat'};
  text = lower(char(value));
  matches = names(startsWith(names, text));
  tf = length(matches) == 1;
end
%=============================================================================
function name = canonicalNameLocal(name)
  switch name
    case 'datavariables'
      name = 'DataVariables';
    otherwise
      name = 'OutputFormat';
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
