%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = movingWindowApply(x, window, operation, fun, varargin)
  % common engine for mov* functions.
  % supported syntaxes forwarded by the wrappers:
  %   mov*(x, k)
  %   mov*(x, [kb kf])
  %   mov*(x, ..., dim)
  %   mov*(x, ..., nanflag)            with 'omitnan' | 'includenan'
  %   mov*(x, ..., 'Endpoints', ep)    with 'shrink' | 'discard' | 'fill'
  % operation names the native sliding statistic ('median', 'mad', 'sum',
  % 'mean', 'min', 'max') or is empty; fun(block, nanflag) reduces the first
  % dimension of a block whose columns are the windows, and is used when the
  % native statistic does not apply.
  narginchk(4, Inf);
  [dim, nanflag, endpoints] = parseMovingOptions(size(x), varargin{:});
  [before, after] = movingWindowSizes(window);
  perm = 1:ndims(x);
  perm([1 dim]) = [dim 1];
  xp = permute(x, perm);
  sz = size(xp);
  n = sz(1);
  cols = prod(sz(2:end));
  xr = reshape(xp, n, cols);
  if strcmp(endpoints, 'discard')
    rows = (1 + before):(n - after);
    sz(1) = length(rows);
  else
    % 'fill' pads incomplete windows with NaN values which are then
    % processed by the aggregation ('omitnan' ignores them, so 'fill'
    % behaves as 'shrink'; 'includenan' propagates them).
    numericFill = isnumeric(endpoints);
    if numericFill
      padValue = endpoints;
    else
      padValue = NaN;
    end
    if numericFill || (strcmp(endpoints, 'fill') && isfloat(x))
      xr = [repmat(padValue, before, cols); xr; repmat(padValue, after, cols)];
      rows = (1 + before):(n + before);
    else
      rows = 1:n;
    end
  end
  if movingWindowIsNative(xr, operation, nanflag)
    yr = movingWindowNative(xr, before, after, rows, operation, nanflag);
  else
    yr = movingWindowRows(xr, before, after, rows, fun, nanflag);
  end
  y = ipermute(reshape(yr, sz), perm);
end
%=============================================================================
function native = movingWindowIsNative(xr, operation, nanflag)
  % the native kernel covers full real arrays of numeric, logical or char
  % values. The mean of single values without their NaN values is
  % accumulated in single precision by the reduction, not by the kernel.
  native = ~isempty(operation) && isreal(xr) && ~issparse(xr) && (isnumeric(xr) || islogical(xr) || ischar(xr));
  if native && strcmp(operation, 'mean') && strcmp(nanflag, 'omitnan') && isa(xr, 'single')
    native = false;
  end
end
%=============================================================================
function yr = movingWindowNative(xr, before, after, rows, operation, nanflag)
  % the native kernel slides over every column at once.
  cols = size(xr, 2);
  if isempty(rows)
    yr = zeros(0, cols);
    return
  end
  if strcmp(operation, 'min') || strcmp(operation, 'max')
    % min and max skip NaN values unless 'includenan' is requested
    omitNaN = ~strcmp(nanflag, 'includenan');
  else
    omitNaN = strcmp(nanflag, 'omitnan') && isfloat(xr);
  end
  if ~isfloat(xr) && ~isinteger(xr)
    xr = double(xr);
  end
  yr = __movwindow__(xr, before, after, rows(1), rows(end), omitNaN, operation);
end
%=============================================================================
function yr = movingWindowRows(xr, before, after, rows, fun, nanflag)
  % row k of yr reduces the window of row rows(k), clipped to the array.
  [n, cols] = size(xr);
  m = length(rows);
  yr = zeros(m, cols);
  complete = (rows - before >= 1) & (rows + after <= n);
  for k = find(~complete)
    r = rows(k);
    yr(k, :) = fun(xr(max(1, r - before):min(n, r + after), :), nanflag);
  end
  first = find(complete, 1);
  last = find(complete, 1, 'last');
  if isempty(first)
    return
  end
  % complete windows form a contiguous range of rows: they are gathered as
  % the columns of a block and reduced at once, chunk by chunk to bound the
  % memory of the block.
  w = before + after + 1;
  offsets = (-before:after)';
  perChunk = max(1, floor(2097152 / w));
  count = last - first + 1;
  if count <= perChunk
    step = max(1, floor(perChunk / count));
    for c1 = 1:step:cols
      c2 = min(cols, c1 + step - 1);
      centers = rows(first:last)' + n * ((c1:c2) - 1);
      block = reshape(xr(offsets + reshape(centers, 1, [])), w, []);
      yr(first:last, c1:c2) = reshape(double(fun(block, nanflag)), count, []);
    end
  else
    for c = 1:cols
      for r1 = first:perChunk:last
        r2 = min(last, r1 + perChunk - 1);
        centers = rows(r1:r2) + n * (c - 1);
        block = reshape(xr(offsets + centers), w, []);
        yr(r1:r2, c) = reshape(double(fun(block, nanflag)), [], 1);
      end
    end
  end
end
%=============================================================================
function [before, after] = movingWindowSizes(window)
  if ~isnumeric(window) || isempty(window) || numel(window) > 2
    error('Nelson:data_analysis:windowPositiveScalarTwoElement', 'Window must be a positive scalar or a two-element vector.');
  end
  if isscalar(window)
    if window < 1
      error('Nelson:data_analysis:windowLengthPositiveScalar', '%s must be a positive scalar.', 'Window length');
    end
    % the window [k - window/2, k + window/2) is centered on element k,
    % also for a noninteger length.
    before = floor(window / 2);
    after = ceil(window / 2) - 1;
  else
    before = window(1);
    after = window(2);
    if before < 0 || after < 0
      error('Nelson:validators:mustBeNonnegative', '%s must be nonnegative.', 'Window values');
    end
    before = floor(before);
    after = floor(after);
  end
end
%=============================================================================
function [dim, nanflag, endpoints] = parseMovingOptions(szx, varargin)
  dim = movingFirstNonsingleton(szx);
  nanflag = '';
  endpoints = 'shrink';
  args = varargin;
  if ~isempty(args) && isnumeric(args{1}) && isscalar(args{1})
    dim = args{1};
    args = args(2:end);
  end
  k = 1;
  while k <= length(args)
    arg = args{k};
    if isstring(arg) && isscalar(arg)
      arg = char(arg);
    end
    if ~ischar(arg)
      error('Nelson:data_analysis:unsupportedOption', 'Unsupported option.');
    end
    if strcmpi(arg, 'omitnan') || strcmpi(arg, 'includenan')
      nanflag = lower(arg);
      k = k + 1;
    elseif strcmpi(arg, 'Endpoints')
      if k + 1 > length(args)
        error('Nelson:data_analysis:valueExpectedAfterEndpoints', 'Value expected after ''Endpoints''.');
      end
      value = args{k + 1};
      if isstring(value) && isscalar(value)
        value = char(value);
      end
      if ischar(value) && any(strcmpi(value, {'shrink', 'discard', 'fill'}))
        endpoints = lower(value);
      elseif isnumeric(value) && isscalar(value)
        % Numeric fill value: incomplete windows are padded with it.
        endpoints = value;
      else
        error('Nelson:data_analysis:unsupportedEndpointsValue', 'Unsupported ''Endpoints'' value.');
      end
      k = k + 2;
    else
      error('Nelson:data_analysis:unsupportedOption', 'Unsupported option.');
    end
  end
end
%=============================================================================
function dim = movingFirstNonsingleton(sz)
  dim = 1;
  for k = 1:length(sz)
    if sz(k) ~= 1
      dim = k;
      return
    end
  end
end
%=============================================================================
