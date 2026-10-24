%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT2 = retime(TT1, newTimes, varargin)
  narginchk(2, Inf);
  parsed = parseRetimeArgumentsLocal(TT1, newTimes, varargin{:});
  newTimes = parsed.NewTimes;
  method = parsed.Method;
  st = struct(TT1);
  oldTimes = st.Properties.RowTimes;
  aggMethods = {'sum', 'mean', 'min', 'max', 'median', 'prod', 'count', ...
    'firstvalue', 'lastvalue'};
  isAggregation = any(strcmp(method, aggMethods));
  tableSt = st.Table;
  outSt = tableSt;
  names = tableSt.Properties.VariableNames;
  % The mapping from old rows to new rows depends only on the time vectors, so
  % it is computed once and reused for every variable (columns only carry data).
  if isAggregation
    ctx = aggregateContextLocal(oldTimes, newTimes);
    for k = 1:length(names)
      name = names{k};
      outSt.data.(name) = aggregateValueLocal(ctx, oldTimes, tableSt.data.(name), newTimes, method);
    end
  else
    ctx = retimeContextLocal(oldTimes, newTimes, method, parsed);
    for k = 1:length(names)
      name = names{k};
      outSt.data.(name) = retimeValueLocal(ctx, tableSt.data.(name), method, parsed);
    end
  end
  TT2 = timetable.fromTableAndTimes(outSt, newTimes, st.Properties);
end
%=============================================================================
function parsed = parseRetimeArgumentsLocal(TT1, newTimes, varargin)
  parsed = struct('Method', 'fillwithmissing', 'NewTimes', [], 'Constant', 0, 'EndValues', 'extrap');
  args = varargin;
  if isRegularKeywordLocal(newTimes)
    if ~isempty(args) && isMethodNameLocal(args{1})
      parsed.Method = lower(char(args{1}));
      args(1) = [];
    end
    [step, args] = parseRegularStepLocal(args);
    oldTimes = TT1.Properties.RowTimes;
    parsed.NewTimes = (floorTimeLocal(min(oldTimes), step):step:max(oldTimes))';
  else
    step = namedStepLocal(newTimes);
    if ~isempty(step)
      oldTimes = TT1.Properties.RowTimes;
      parsed.NewTimes = (floorTimeLocal(min(oldTimes), step):step:max(oldTimes))';
    else
      parsed.NewTimes = columnTimesLocal(newTimes);
    end
  end
  if ~isRegularKeywordLocal(newTimes) && ~isempty(args) && isMethodNameLocal(args{1})
    parsed.Method = lower(char(args{1}));
    args(1) = [];
  end
  parsed = parseRetimeOptionsLocal(parsed, args{:});
end
%=============================================================================
function tf = isRegularKeywordLocal(value)
  tf = (ischar(value) || (isstring(value) && isscalar(value))) && strcmpi(char(value), 'regular');
end
%=============================================================================
function [step, args] = parseRegularStepLocal(args)
  if ~isempty(args) && ~isMethodNameLocal(args{1}) && ~isOptionNameLocal(args{1})
    step = namedStepLocal(args{1});
    if isempty(step)
      error('Nelson:timetable:unknownTimeStep', 'Unknown regular timetable time step.');
    end
    args(1) = [];
    return
  end
  step = [];
  k = 1;
  while k < numel(args)
    name = args{k};
    value = args{k + 1};
    if isTextScalarLocal(name) && strcmpi(char(name), 'TimeStep')
      step = value;
      args(k:k + 1) = [];
      return
    elseif isTextScalarLocal(name) && strcmpi(char(name), 'SampleRate')
      if ~(isnumeric(value) && isscalar(value) && value > 0)
        error('Nelson:timetable:invalidValue:sampleRateAPositiveNumericScalar', 'SampleRate must be a positive numeric scalar.');
      end
      step = seconds(1 / double(value));
      args(k:k + 1) = [];
      return
    end
    k = k + 2;
  end
  error('Nelson:timetable:invalidSyntax:regularRetimingRequiresTimeStepSampleRate', 'Regular retiming requires TimeStep or SampleRate.');
end
%=============================================================================
function parsed = parseRetimeOptionsLocal(parsed, varargin)
  if mod(numel(varargin), 2) ~= 0
    error('Nelson:timetable:nameValuePair', 'Name-value arguments must come in pairs.');
  end
  for k = 1:2:numel(varargin)
    name = varargin{k};
    if ~isTextScalarLocal(name)
      error('Nelson:timetable:nameValuePair:nameValueArgumentNamesTextScalars', 'Name-value argument names must be text scalars.');
    end
    switch lower(char(name))
      case 'constant'
        parsed.Constant = varargin{k + 1};
      case 'endvalues'
        parsed.EndValues = varargin{k + 1};
      otherwise
        error('Nelson:timetable:unknownOption', 'Unknown option ''%s''.', char(name));
    end
  end
end
%=============================================================================
function tf = isMethodNameLocal(value)
  if ~isTextScalarLocal(value)
    tf = false;
    return
  end
  methods = {'fillwithmissing', 'fillwithconstant', 'nearest', 'previous', 'next', ...
    'linear', 'spline', 'pchip', 'makima', 'sum', 'mean', 'min', 'max', ...
    'median', 'prod', 'count', 'firstvalue', 'lastvalue'};
  tf = any(strcmpi(char(value), methods));
end
%=============================================================================
function tf = isOptionNameLocal(value)
  tf = isTextScalarLocal(value) && any(strcmpi(char(value), {'TimeStep', 'SampleRate', 'Constant', 'EndValues'}));
end
%=============================================================================
function step = namedStepLocal(value)
  step = [];
  if ~(ischar(value) || (isstring(value) && isscalar(value)))
    return
  end
  switch lower(char(value))
    case 'yearly'
      step = calyears(1);
    case 'quarterly'
      step = calquarters(1);
    case 'monthly'
      step = calmonths(1);
    case 'weekly'
      step = calweeks(1);
    case 'daily'
      step = caldays(1);
    case 'hourly'
      step = hours(1);
    case 'minutely'
      step = minutes(1);
    case 'secondly'
      step = seconds(1);
  end
end
%=============================================================================
function t = floorTimeLocal(t, step)
  if isa(t, 'datetime') && isa(step, 'duration')
    secondsPerStep = seconds(step);
    serial = floor(datenum(t) .* 86400 ./ secondsPerStep) .* secondsPerStep ./ 86400;
    t = datetime(serial, 'ConvertFrom', 'datenum', 'Format', t.Format, 'TimeZone', t.TimeZone);
  elseif isa(t, 'duration') && isa(step, 'duration')
    t = seconds(floor(seconds(t) ./ seconds(step)) .* seconds(step));
  end
end
%=============================================================================
function ctx = aggregateContextLocal(oldTimes, newTimes)
  % Precompute, from the time vectors alone, the assignment of each old row to
  % its aggregation bin. Each old row lands in exactly one bin, so the work is
  % linear instead of rescanning all old times for every new row.
  ctx.n = timeLengthLocal(newTimes);
  ctx.xOld = timeAsDoubleLocal(oldTimes);
  ctx.xNew = timeAsDoubleLocal(newTimes);
  ctx.fast = ctx.n >= 1 && all(diff(ctx.xNew(:)) > 0);
  ctx.segStart = [];
  ctx.segEnd = [];
  ctx.segBin = [];
  ctx.segRows = [];
  if ~ctx.fast
    return
  end
  % Bin each old row: bins(i) = number of new times <= xOld(i). That is the
  % discretize([xNew; Inf]) bin index (0 means the old row is before the first
  % new time and is dropped), computed in O((m+n)*log) instead of O(m*n).
  bins = countCompareLocal(ctx.xNew, ctx.xOld, true);
  valid = bins > 0;
  origIdx = find(valid);
  bv = bins(valid);
  if isempty(bv)
    return
  end
  % Group old rows by bin, keeping original row order within each bin (needed by
  % firstvalue/lastvalue). sortrows breaks ties on the original index ascending.
  M = sortrows([bv(:), origIdx(:)]);
  sb = M(:, 1);
  ctx.segRows = M(:, 2);
  ctx.segStart = [1; find(diff(sb) ~= 0) + 1];
  ctx.segEnd = [ctx.segStart(2:end) - 1; numel(sb)];
  ctx.segBin = sb(ctx.segStart);
end
%=============================================================================
function value = aggregateValueLocal(ctx, oldTimes, oldValue, newTimes, method)
  if ~ctx.fast
    value = aggregateValueRefLocal(oldTimes, oldValue, newTimes, method);
    return
  end
  if strcmp(method, 'count')
    value = zeros(ctx.n, size(oldValue, 2));
  else
    value = defaultRowsLocal(oldValue, ctx.n);
  end
  segStart = ctx.segStart;
  segEnd = ctx.segEnd;
  segBin = ctx.segBin;
  segRows = ctx.segRows;
  for g = 1:numel(segStart)
    rows = segRows(segStart(g):segEnd(g));
    value(segBin(g), :) = reduceAggLocal(method, oldValue(rows, :));
  end
end
%=============================================================================
function r = reduceAggLocal(method, sub)
  switch method
    case 'sum'
      r = sum(sub, 1);
    case 'mean'
      r = mean(sub, 1);
    case 'min'
      r = min(sub, [], 1);
    case 'max'
      r = max(sub, [], 1);
    case 'median'
      r = median(sub, 1);
    case 'prod'
      r = prod(sub, 1);
    case 'count'
      r = sum(~ismissing(sub), 1);
    case 'firstvalue'
      r = sub(1, :);
    case 'lastvalue'
      r = sub(end, :);
  end
end
%=============================================================================
function value = aggregateValueRefLocal(oldTimes, oldValue, newTimes, method)
  % Reference loop kept for non strictly increasing new times (duplicate or
  % unsorted bin edges), where bin assignment cannot be vectorized safely.
  n = timeLengthLocal(newTimes);
  value = defaultRowsLocal(oldValue, n);
  if strcmp(method, 'count')
    value = zeros(n, size(oldValue, 2));
  end
  for k = 1:n
    if k < n
      mask = (oldTimes >= newTimes(k)) & (oldTimes < newTimes(k + 1));
    else
      mask = (oldTimes >= newTimes(k));
    end
    sub = oldValue(mask, :);
    if isempty(sub) && ~strcmp(method, 'count')
      continue
    end
    value(k, :) = reduceAggLocal(method, sub);
  end
end
%=============================================================================
function ctx = retimeContextLocal(oldTimes, newTimes, method, parsed)
  % Precompute, from the time vectors alone, the exact-match map and the
  % method specific old-row index (or interpolation bounds) shared by columns.
  ctx.n = timeLengthLocal(newTimes);
  ctx.xOld = timeAsDoubleLocal(oldTimes);
  ctx.xNew = timeAsDoubleLocal(newTimes);
  ctx.nearIdx = [];
  ctx.prevIdx = [];
  ctx.nextIdx = [];
  ctx.xmin = [];
  ctx.xmax = [];
  ctx.extrapMode = true;
  % Exact match has priority for every method. Compare on the time objects (not
  % the double projection) to reproduce the exact == semantics, and take the
  % first occurrence of each matched time.
  [uOld, iaOld] = unique(oldTimes, 'first');
  [tf, loc] = ismember(newTimes, uOld);
  tf = tf(:);
  loc = loc(:);
  ctx.exactRows = find(tf);
  ctx.exactOldIdx = iaOld(loc(ctx.exactRows));
  ctx.notExact = ~tf;
  switch method
    case 'nearest'
      ctx.nearIdx = nearestIndexVecLocal(ctx.xOld, ctx.xNew);
    case {'previous', 'next'}
      [ctx.prevIdx, ctx.nextIdx] = prevNextIndexVecLocal(ctx.xOld, ctx.xNew);
    case {'linear', 'spline', 'pchip', 'makima'}
      ctx.xmin = min(ctx.xOld);
      ctx.xmax = max(ctx.xOld);
      ctx.extrapMode = isExtrapEndValuesLocal(parsed.EndValues);
  end
end
%=============================================================================
function value = retimeValueLocal(ctx, oldValue, method, parsed)
  value = defaultRowsLocal(oldValue, ctx.n);
  if strcmp(method, 'fillwithconstant')
    value = fillConstantRowsLocal(value, parsed.Constant);
  end
  switch method
    case 'nearest'
      valid = ctx.nearIdx > 0;
      value(valid, :) = oldValue(ctx.nearIdx(valid), :);
    case 'previous'
      valid = ctx.prevIdx > 0;
      value(valid, :) = oldValue(ctx.prevIdx(valid), :);
    case 'next'
      valid = ctx.nextIdx > 0;
      value(valid, :) = oldValue(ctx.nextIdx(valid), :);
    case {'linear', 'spline', 'pchip', 'makima'}
      value = interpColumnLocal(ctx, oldValue, method, parsed, value);
    case 'fillwithmissing'
    case 'fillwithconstant'
    otherwise
      error('Nelson:timetable:unsupportedMethod', 'Unsupported retime method.');
  end
  % Exact-match override wins over the method result for every method.
  if ~isempty(ctx.exactRows)
    value(ctx.exactRows, :) = oldValue(ctx.exactOldIdx, :);
  end
end
%=============================================================================
function value = interpColumnLocal(ctx, oldValue, method, parsed, value)
  if ~isnumeric(oldValue)
    return
  end
  rowsI = find(ctx.notExact);
  if isempty(rowsI)
    return
  end
  xi = ctx.xNew(rowsI);
  if ctx.extrapMode
    value(rowsI, :) = interpAtLocal(ctx.xOld, oldValue, xi, method);
    return
  end
  % With constant end values, out-of-range points are filled without touching
  % interp1 (which never sees them in the per-point reference implementation).
  oor = xi < ctx.xmin | xi > ctx.xmax;
  inr = ~oor;
  if any(inr)
    value(rowsI(inr), :) = interpAtLocal(ctx.xOld, oldValue, xi(inr), method);
  end
  if isnumeric(parsed.EndValues) || islogical(parsed.EndValues)
    value(rowsI(oor), :) = parsed.EndValues;
  end
end
%=============================================================================
function yi = interpAtLocal(xOld, oldValue, xi, method)
  % interp1 on the whole query vector for floating-point data; a per-point loop
  % for integer-class data, which interp1 only accepts a scalar query point for.
  if isfloat(oldValue)
    yi = interp1(xOld, oldValue, xi, method, 'extrap');
    return
  end
  yi = cast(zeros(numel(xi), size(oldValue, 2)), class(oldValue));
  for i = 1:numel(xi)
    yi(i, :) = interp1(xOld, oldValue, xi(i), method, 'extrap');
  end
end
%=============================================================================
function idx = nearestIndexVecLocal(xOld, xNew)
  % For each new time, the old-row index whose time is closest (distances on the
  % double projection), breaking ties by lowest original row index to match the
  % first-index behaviour of min(abs(...)).
  xOld = xOld(:);
  xNew = xNew(:);
  nq = numel(xNew);
  idx = zeros(nq, 1);
  m = numel(xOld);
  if m == 0
    return
  end
  [uv, ~, g] = unique(xOld);
  minIdx = accumarray(g, (1:m)', [], @min);
  mu = numel(uv);
  loPos = countCompareLocal(uv, xNew, true);
  hiPos = countCompareLocal(uv, xNew, false) + 1;
  hasLo = loPos >= 1;
  hasHi = hiPos <= mu;
  loPosG = max(loPos, 1);
  hiPosG = min(hiPos, mu);
  dLo = inf(nq, 1);
  dHi = inf(nq, 1);
  dLo(hasLo) = xNew(hasLo) - uv(loPosG(hasLo));
  dHi(hasHi) = uv(hiPosG(hasHi)) - xNew(hasHi);
  idxLo = minIdx(loPosG);
  idxHi = minIdx(hiPosG);
  useLo = hasLo & (~hasHi | dLo < dHi | (dLo == dHi & idxLo <= idxHi));
  idx(useLo) = idxLo(useLo);
  useHi = ~useLo;
  idx(useHi) = idxHi(useHi);
end
%=============================================================================
function [prevIdx, nextIdx] = prevNextIndexVecLocal(xOld, xNew)
  % previous/next take the last/first old row by original position among rows
  % whose time is <= / >= the new time. Sorting old times ascending turns that
  % into a running max / min of original indices over a prefix / suffix.
  xOld = xOld(:);
  xNew = xNew(:);
  nq = numel(xNew);
  m = numel(xOld);
  prevIdx = zeros(nq, 1);
  nextIdx = zeros(nq, 1);
  if m == 0
    return
  end
  [~, ord] = sort(xOld);
  cmax = prefixMaxLocal(ord);
  cminRev = flipud(prefixMinLocal(flipud(ord)));
  p = countCompareLocal(xOld, xNew, true);
  hasP = p >= 1;
  prevIdx(hasP) = cmax(p(hasP));
  s = countCompareLocal(xOld, xNew, false) + 1;
  hasS = s <= m;
  nextIdx(hasS) = cminRev(s(hasS));
end
%=============================================================================
function c = prefixMaxLocal(v)
  % Inclusive running maximum via Hillis-Steele doubling: O(n*log n) using
  % vectorized element-wise max, as cummax is orders of magnitude slower here.
  c = v(:);
  n = numel(c);
  s = 1;
  while s < n
    c(s + 1:n) = max(c(s + 1:n), c(1:n - s));
    s = s * 2;
  end
end
%=============================================================================
function c = prefixMinLocal(v)
  c = v(:);
  n = numel(c);
  s = 1;
  while s < n
    c(s + 1:n) = min(c(s + 1:n), c(1:n - s));
    s = s * 2;
  end
end
%=============================================================================
function cnt = countCompareLocal(a, q, includeEqual)
  % Vectorized searchsorted: cnt(i) = number of entries of a that are <= q(i)
  % (includeEqual true) or strictly < q(i) (includeEqual false). O((m+n)*log).
  a = a(:);
  q = q(:);
  m = numel(a);
  n = numel(q);
  both = [a; q];
  if includeEqual
    sec = [zeros(m, 1); ones(n, 1)];
  else
    sec = [ones(m, 1); zeros(n, 1)];
  end
  [~, sidx] = sortrows([both, sec]);
  isA = [true(m, 1); false(n, 1)];
  isASorted = isA(sidx);
  cumA = cumsum(isASorted);
  isQSorted = ~isASorted;
  cntSorted = cumA(isQSorted);
  qOrig = sidx(isQSorted) - m;
  cnt = zeros(n, 1);
  cnt(qOrig) = cntSorted;
end
%=============================================================================
function n = timeLengthLocal(value)
  if isa(value, 'datetime')
    n = numel(datenum(value));
  elseif isa(value, 'duration')
    n = numel(seconds(value));
  else
    n = length(value);
  end
end
%=============================================================================
function tf = isExtrapEndValuesLocal(value)
  tf = isTextScalarLocal(value) && strcmpi(char(value), 'extrap');
end
%=============================================================================
function value = fillConstantRowsLocal(value, constant)
  if isnumeric(value) || islogical(value)
    value(:) = constant;
  elseif iscell(value)
    value(:) = {constant};
  end
end
%=============================================================================
function x = timeAsDoubleLocal(t)
  if isa(t, 'datetime')
    x = datenum(t);
  elseif isa(t, 'duration')
    x = double(t);
  else
    x = double(t);
  end
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
function value = defaultRowsLocal(sample, n)
  trailing = size(sample);
  if isempty(trailing)
    trailing = [0 0];
  end
  cols = trailing(2:end);
  if isempty(cols)
    cols = 1;
  end
  if isa(sample, 'datetime')
    value = NaT([n, cols]);
  elseif isa(sample, 'duration')
    value = seconds(NaN([n, cols]));
  elseif isnumeric(sample)
    value = NaN([n, cols]);
    try
      value = cast(value, class(sample));
    catch
    end
  elseif islogical(sample)
    value = false([n, cols]);
  elseif isstring(sample)
    value = strings([n, cols]);
  elseif iscell(sample)
    value = cell([n, cols]);
  elseif ischar(sample)
    value = char(zeros([n, cols]));
  else
    value = sample([]);
  end
end
%=============================================================================
function value = columnTimesLocal(value)
  if isa(value, 'datetime') || isa(value, 'duration')
    value = value(:);
  elseif isrow(value)
    value = value(:);
  end
end
%=============================================================================
