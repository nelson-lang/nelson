%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, FB] = fillmissing(A, method, varargin)
  narginchk(2, Inf);
  opts = parseFillmissingInputs(method, varargin);
  if istable(A) || istimetable(A)
    [B, FB] = fillTable(A, opts);
  else
    [B, FB] = fillArray(A, opts);
  end
  if nargout < 2
    clear FB;
  end
end
%=============================================================================
function opts = parseFillmissingInputs(method, args)
  opts = struct();
  opts.Method = checkFillMethod(method);
  opts.Value = [];
  opts.ValueProvided = false;
  opts.MissingLocations = [];
  opts.EndValues = 'extrap';
  opts.SamplePoints = [];
  opts.MaxGap = [];
  opts.DataVariables = [];
  opts.ReplaceValues = true;
  opts.Dim = [];
  opts.Distance = 'euclidean';
  isConstant = strcmp(opts.Method, 'constant');
  isKnn = strcmp(opts.Method, 'knn');
  takesValue = isConstant || isKnn || isa(opts.Method, 'function_handle') || any(strcmp(opts.Method, {'movmean', 'movmedian'}));
  % The fill value of 'constant' may itself be text; the window of the
  % moving and function handle methods and k of 'knn' are numeric.
  if takesValue && ~isempty(args) && (isConstant || ~isText(args{1}))
    opts.Value = args{1};
    opts.ValueProvided = true;
    args(1) = [];
  end
  if isKnn
    if ~opts.ValueProvided
      opts.Value = 1;
    end
    k = opts.Value;
    if ~(isnumeric(k) && isscalar(k) && isreal(k) && k >= 1 && fix(k) == k)
      error('Nelson:data_analysis:knnInvalidK', '''k'' value must be a real, positive integer.');
    end
  elseif takesValue && ~opts.ValueProvided
    if isConstant
      error('Nelson:data_analysis:fillValueExpected', 'Fill value expected.');
    end
    error('Nelson:data_analysis:windowSizeExpected', '%s expected.', 'Window size');
  end
  if ~isempty(args) && ~isText(args{1})
    opts.Dim = args{1};
    args(1) = [];
    if ~(isnumeric(opts.Dim) && isscalar(opts.Dim) && isreal(opts.Dim) && opts.Dim >= 1 && fix(opts.Dim) == opts.Dim)
      error('Nelson:data_analysis:dimensionArgumentPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Dimension argument');
    end
  end
  provided = {};
  k = 1;
  while k <= length(args)
    name = args{k};
    if isstring(name)
      name = char(name);
    end
    if ~ischar(name)
      error('Nelson:data_analysis:nameValueArgumentExpected', 'Name-value argument expected.');
    end
    if k == length(args)
      error('Nelson:data_analysis:nameValueArgumentFollowedValue', 'Name-value argument must be followed by a value.');
    end
    provided{end + 1} = lower(name);
    switch lower(name)
      case 'missinglocations'
        opts.MissingLocations = args{k + 1};
      case 'endvalues'
        opts.EndValues = args{k + 1};
        if isstring(opts.EndValues)
          opts.EndValues = char(opts.EndValues);
        end
      case 'samplepoints'
        opts.SamplePoints = args{k + 1};
      case 'maxgap'
        opts.MaxGap = args{k + 1};
      case 'datavariables'
        opts.DataVariables = args{k + 1};
      case 'replacevalues'
        opts.ReplaceValues = logical(args{k + 1});
      case 'distance'
        opts.Distance = checkKnnDistance(args{k + 1});
      otherwise
        error('Nelson:data_analysis:unknownOption', 'Unknown option.');
    end
    k = k + 2;
  end
  checkKnnOptions(isKnn, provided);
end
%=============================================================================
function distance = checkKnnDistance(distance)
  if isa(distance, 'function_handle')
    return
  end
  if isText(distance)
    distance = lower(char(distance));
  end
  if ~(ischar(distance) && any(strcmp(distance, {'euclidean', 'seuclidean'})))
    error('Nelson:data_analysis:knnInvalidDistance', '''Distance'' value must be ''euclidean'', ''seuclidean'', or a function handle.');
  end
end
%=============================================================================
function checkKnnOptions(isKnn, provided)
  if ~isKnn
    if any(strcmp(provided, 'distance'))
      error('Nelson:data_analysis:distanceOnlyForKnn', 'The ''Distance'' argument is supported only for the ''knn'' method.');
    end
    return
  end
  names = {'endvalues', 'maxgap', 'samplepoints'};
  labels = {'EndValues', 'MaxGap', 'SamplePoints'};
  for k = 1:length(names)
    if any(strcmp(provided, names{k}))
      error('Nelson:data_analysis:knnUnsupportedArgument', 'The %s argument is not supported for the ''knn'' method.', labels{k});
    end
  end
end
%=============================================================================
function method = checkFillMethod(method)
  if isa(method, 'function_handle')
    return
  end
  if ~isText(method)
    error('Nelson:data_analysis:unsupportedFillMethod', 'Unsupported "%s" option.', 'method');
  end
  method = lower(char(method));
  supported = {'constant', 'previous', 'next', 'nearest', 'linear', 'spline', 'pchip', 'makima', 'movmean', 'movmedian', 'mean', 'median', 'mode', 'knn'};
  if ~any(strcmp(method, supported))
    error('Nelson:data_analysis:unsupportedFillMethod', 'Unsupported "%s" option.', method);
  end
end
%=============================================================================
function tf = isText(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
function [B, FB] = fillTable(A, opts)
  B = A;
  if ~isempty(opts.Dim)
    error('Nelson:data_analysis:fillmissingDimensionTable', 'Dimension parameter not supported when first argument is a table or timetable.');
  end
  if istimetable(A)
    % The row times are the sample points of a timetable.
    if ~isempty(opts.SamplePoints)
      error('Nelson:data_analysis:fillmissingSamplePointsTimetable', '''SamplePoints'' parameter not supported when first argument is a timetable.');
    end
    opts.SamplePoints = A.Properties.RowTimes;
  end
  if ~isempty(opts.MissingLocations) && ~istable(opts.MissingLocations) && ~(islogical(opts.MissingLocations) && isequal(size(opts.MissingLocations), [height(A), width(A)]))
    missingLocationsError();
  end
  variableNames = A.Properties.VariableNames;
  if isempty(opts.DataVariables)
    dataVars = 1:width(A);
  else
    stA = struct(A);
    dataVars = tableResolveVariables(stA.Properties.VariableNames, opts.DataVariables, stA.Properties.VariableTypes, stA.data);
  end
  % Table variables are filled along their rows.
  varOpts = opts;
  varOpts.Dim = 1;
  nVars = length(dataVars);
  % A vector of fill values or end values holds one value per data variable.
  checkSliceValues(opts, nVars);
  if strcmp(opts.Method, 'knn')
    [filledValues, filledMasks] = fillTableKnn(A, varOpts, variableNames(dataVars), dataVars);
  else
    filledValues = cell(1, nVars);
    filledMasks = cell(1, nVars);
    for position = 1:nVars
      name = variableNames{dataVars(position)};
      varOpts.MissingLocations = variableMissingLocations(opts.MissingLocations, name, dataVars(position));
      varOpts.Value = valueForVariable(opts.Value, position);
      varOpts.EndValues = valueForVariable(opts.EndValues, position);
      [filledValues{position}, filledMasks{position}] = fillArray(A.(name), varOpts);
    end
  end
  % TF has one column per variable of B, true where a row of the variable was filled.
  if opts.ReplaceValues
    FB = false(height(A), width(A));
  else
    FB = false(height(A), width(A) + nVars);
  end
  for position = 1:nVars
    name = variableNames{dataVars(position)};
    if opts.ReplaceValues
      B.(name) = filledValues{position};
      FB(:, dataVars(position)) = any(filledMasks{position}, 2);
    else
      B.([name, '_filled']) = filledValues{position};
      FB(:, width(A) + position) = any(filledMasks{position}, 2);
    end
  end
end
%=============================================================================
function locations = variableMissingLocations(missingLocations, name, v)
  locations = [];
  if isempty(missingLocations)
    return
  end
  if istable(missingLocations)
    locations = missingLocations.(name);
  else
    locations = missingLocations(:, v);
  end
end
%=============================================================================
function [filledValues, filledMasks] = fillTableKnn(A, opts, names, dataVars)
  % 'knn' compares whole rows: the data variables are filled together as the columns of one matrix.
  nVars = length(names);
  values = cell(1, nVars);
  locations = cell(1, nVars);
  for position = 1:nVars
    values{position} = A.(names{position});
    if size(values{position}, 2) ~= 1
      error('Nelson:data_analysis:knnMultiColumnVariable', 'Table variables with multiple columns are not supported for the ''knn'' method.');
    end
    checkKnnInput(values{position}, opts.Distance);
    locations{position} = variableMissingLocations(opts.MissingLocations, names{position}, dataVars(position));
  end
  opts.MissingLocations = [locations{:}];
  [filled, mask] = fillArray([values{:}], opts);
  filledValues = cell(1, nVars);
  filledMasks = cell(1, nVars);
  for position = 1:nVars
    filledValues{position} = cast(filled(:, position), class(values{position}));
    filledMasks{position} = mask(:, position);
  end
end
%=============================================================================
function value = valueForVariable(values, position)
  % Fill or end value of the data variable at position (vectors and cells hold one per variable).
  value = values;
  if iscell(values) && ~isscalar(values)
    value = values{position};
  elseif iscell(values)
    value = values{1};
  elseif (isnumeric(values) || islogical(values)) && ~isscalar(values) && ~isempty(values)
    value = values(position);
  end
end
%=============================================================================
function checkSliceValues(opts, slices)
  % The fill constant and numeric end values are a scalar or one value per slice.
  if strcmp(opts.Method, 'constant') && ~(ischar(opts.Value) || isscalar(opts.Value) || (isvector(opts.Value) && numel(opts.Value) == slices))
    error('Nelson:data_analysis:fillmissingConstantSize', 'Fill constant must be a scalar or a %d-element vector.', slices);
  end
  if ~ischar(opts.EndValues) && ~(isscalar(opts.EndValues) || (isvector(opts.EndValues) && numel(opts.EndValues) == slices))
    error('Nelson:data_analysis:fillmissingEndValuesSize', '''EndValues'' value must be a scalar or a %d-element vector.', slices);
  end
end
%=============================================================================
function missingLocationsError()
  error('Nelson:data_analysis:fillmissingInvalidMissingLocations', '''MissingLocations'' value must be a logical array of the same size as the first argument.');
end
%=============================================================================
function [B, FB] = fillArray(A, opts)
  % Every method works on the columns of X: the slices of A along dim.
  B = A;
  if isempty(opts.MissingLocations)
    M = ismissing(A);
  else
    M = opts.MissingLocations;
    if ~(islogical(M) && isequal(size(M), size(A)))
      missingLocationsError();
    end
    if (isinteger(A) || islogical(A)) && any(strcmp(opts.Method, {'linear', 'spline', 'pchip', 'makima', 'movmean', 'movmedian'}))
      error('Nelson:data_analysis:fillmissingIntegerMissingLocations', 'Fill method must be ''constant'', ''previous'', ''next'', ''nearest'', ''knn'', or a function handle when using ''MissingLocations'' with integer or logical inputs.');
    end
  end
  sz = size(A);
  dim = opts.Dim;
  if isempty(dim)
    dim = find(sz ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  sz(end + 1:dim) = 1;
  n = sz(dim);
  opts = checkSliceOptions(opts, n, prod(sz([1:dim - 1, dim + 1:end])));
  isExtrap = ischar(opts.EndValues) && strcmpi(opts.EndValues, 'extrap');
  if strcmp(opts.Method, 'constant') && isExtrap && isempty(opts.MaxGap) && isscalar(opts.Value) && ~iscell(A)
    % A scalar fill value does not depend on the slice.
    B(M) = opts.Value;
    FB = reshape(M & ~ismissing(opts.Value), size(A));
    return
  end
  if strcmp(opts.Method, 'knn')
    checkKnnInput(A, opts.Distance);
  end
  FB = false(size(A));
  if ~any(M(:))
    return
  end
  checkMethodType(A, opts.Method);
  % Moving dim first is a plain reshape when the dimensions before it are singleton.
  perm = [dim, 1:dim - 1, dim + 1:length(sz)];
  mustPermute = any(sz(1:dim - 1) ~= 1);
  if mustPermute
    X = reshape(permute(A, perm), n, []);
    M = reshape(permute(M, perm), n, []);
  else
    X = reshape(A, n, []);
    M = reshape(M, n, []);
  end
  if isExtrap && isempty(opts.MaxGap) && any(strcmp(opts.Method, {'mean', 'median', 'mode'}))
    [X, filled] = fillSliceStatistic(X, M, opts.Method);
  else
    [X, filled] = fillMissingEntries(X, M, opts, isExtrap);
  end
  if mustPermute
    B = ipermute(reshape(X, sz(perm)), perm);
    FB = ipermute(reshape(filled, sz(perm)), perm);
  else
    B = reshape(X, size(A));
    FB = reshape(filled, size(A));
  end
end
%=============================================================================
function opts = checkSliceOptions(opts, n, slices)
  % Validates the options depending on the n entries of each slice and on
  % the number of slices; sample points, MaxGap and windows become numeric
  % (seconds for datetime and duration values). SampleValues keeps the
  % sample points as given, for the function handle method.
  checkSliceValues(opts, slices);
  isFunction = isa(opts.Method, 'function_handle');
  hasWindow = isFunction || any(strcmp(opts.Method, {'movmean', 'movmedian'}));
  window = [];
  if hasWindow
    window = opts.Value;
  end
  opts.SampleValues = opts.SamplePoints(:);
  if ~hasWindow && isempty(opts.SamplePoints) && isempty(opts.MaxGap)
    return
  end
  [opts.SamplePoints, opts.MaxGap, window] = fillmissingSamplePoints(opts.SamplePoints, n, opts.MaxGap, window, isFunction);
  if ~hasWindow
    return
  end
  finite = isnumeric(window) && isreal(window) && all(isfinite(window));
  if isFunction
    if ~(finite && ((isscalar(window) && window > 0) || (numel(window) == 2 && all(window >= 0) && any(window > 0))))
      error('Nelson:data_analysis:fillmissingInvalidGapWindow', 'Window length must be a finite positive scalar or 2-element vector of finite nonnegative scalars.');
    end
  elseif ~(finite && ((isscalar(window) && window > 0) || (numel(window) == 2 && all(window >= 0))))
    error('Nelson:data_analysis:windowPositiveScalarTwoElement', 'Window must be a positive scalar or a two-element vector.');
  end
  opts.Value = window;
end
%=============================================================================
function [X, filled] = fillMissingEntries(X, M, opts, isExtrap)
  % Generic path: a value is proposed for each missing entry, then EndValues and MaxGap apply.
  n = size(X, 1);
  % Sample points are only built for the methods measuring distances.
  usesX = ~isempty(opts.MaxGap) || ~any(strcmp(opts.Method, {'constant', 'previous', 'next', 'mean', 'median', 'mode', 'movmean', 'movmedian', 'knn'}));
  if ~isempty(opts.SamplePoints)
    x = double(opts.SamplePoints(:));
  elseif usesX
    x = (1:n)';
  else
    x = [];
  end
  withNeighbors = ~isExtrap || ~isempty(opts.MaxGap) || any(strcmp(opts.Method, {'previous', 'next', 'nearest', 'linear'}));
  loc = missingEntries(M, withNeighbors);
  [Fm, fm] = fillValues(X, M, x, loc, opts);
  if ~isExtrap
    [Fm, fm] = applyEndValues(X, Fm, fm, loc, opts.EndValues);
  end
  if ~isempty(opts.MaxGap)
    fm = fm & ~gapTooLarge(x, loc, opts.MaxGap);
  end
  idx = loc.miss(fm);
  X(idx) = Fm(fm);
  % An entry filled with a missing value does not count as filled.
  filled = false(size(M));
  filled(idx) = ~ismissing(X(idx));
end
%=============================================================================
function [X, filled] = fillSliceStatistic(X, M, method)
  % Default path of the statistic methods: one value per slice, assigned through the mask.
  stat = sliceStatistic(X, M, method);
  if isscalar(stat)
    filled = M & ~isnan(stat);
    X(filled) = stat;
  else
    F = repmat(stat, size(X, 1), 1);
    filled = M & ~isnan(F);
    X(filled) = F(filled);
  end
end
%=============================================================================
function checkKnnInput(A, distance)
  if ndims(A) > 2
    error('Nelson:data_analysis:knnMatrixExpected', 'First argument must be a matrix or a table for the ''knn'' method.');
  end
  if isa(distance, 'function_handle')
    return
  end
  if ~isfloat(A)
    error('Nelson:data_analysis:knnFloatExpected', 'The ''euclidean'' and ''seuclidean'' distance metrics support only double and single data.');
  end
  if ~isreal(A)
    error('Nelson:data_analysis:knnRealExpected', 'First argument must be real when using the ''euclidean'' and ''seuclidean'' distance metrics.');
  end
end
%=============================================================================
function checkMethodType(A, method)
  numericOnly = {'knn', 'linear', 'spline', 'pchip', 'makima', 'movmean', 'movmedian', 'mean', 'median', 'mode'};
  if ischar(method) && any(strcmp(method, numericOnly)) && ~(isnumeric(A) || islogical(A))
    error('Nelson:data_analysis:unsupportedFillMethod', 'Unsupported "%s" option.', method);
  end
end
%=============================================================================
function loc = missingEntries(M, withNeighbors)
  % Linear index, row and column of the missing entries of M (column-major).
  % With neighbors: p lists the nonmissing entries, gpos(k) counts those
  % before missing entry k, and the nonmissing entries of its column are
  % p(lo(k) + 1:hi(k)).
  n = size(M, 1);
  loc.n = n;
  loc.miss = find(M(:));
  if size(M, 2) == 1
    loc.col = ones(size(loc.miss));
    loc.row = loc.miss;
  else
    loc.col = floor((loc.miss - 1) / n) + 1;
    loc.row = loc.miss - (loc.col - 1) * n;
  end
  if withNeighbors
    known = ~M(:);
    loc.p = find(known);
    g = cumsum(double(known));
    loc.gpos = g(loc.miss);
    counts = sum(~M, 1);
    stops = cumsum(counts);
    loc.hi = reshape(stops(loc.col), [], 1);
    loc.lo = loc.hi - reshape(counts(loc.col), [], 1);
  end
end
%=============================================================================
function idx = knownIndex(loc, pos, valid)
  % Linear index of the nonmissing entry p(pos) where valid, the missing entry itself elsewhere.
  idx = loc.miss;
  idx(valid) = loc.p(pos(valid));
end
%=============================================================================
function r = rowOf(loc, idx)
  r = idx - (loc.col - 1) * loc.n;
end
%=============================================================================
function [Fm, fm] = fillValues(X, M, x, loc, opts)
  % Fm(k) is the value proposed for the missing entry loc.miss(k), used where fm(k).
  method = opts.Method;
  if isa(method, 'function_handle')
    t = opts.SampleValues;
    if isempty(t)
      t = x;
    end
    [Fm, fm] = fillFunction(X, x, t, loc, method, opts.Value);
    return
  end
  switch method
    case 'constant'
      [Fm, fm] = fillConstant(X, loc, opts.Value);
    case 'previous'
      fm = loc.gpos > loc.lo;
      Fm = reshape(X(knownIndex(loc, loc.gpos, fm)), [], 1);
    case 'next'
      fm = loc.gpos < loc.hi;
      Fm = reshape(X(knownIndex(loc, loc.gpos + 1, fm)), [], 1);
    case 'nearest'
      [Fm, fm] = fillNearest(X, x, loc);
    case 'linear'
      [Fm, fm] = fillLinear(X, x, loc);
    case {'spline', 'pchip', 'makima'}
      [Fm, fm] = fillInterp(X, M, x, loc, method);
    case 'knn'
      [Fm, fm] = fillmissingKnn(X, M, loc, opts.Value, opts.Distance);
    case {'movmean', 'movmedian'}
      [Fm, fm] = fillMoving(X, M, x, loc, method, opts.Value, opts.SamplePoints);
    otherwise
      [Fm, fm] = fillStatistic(X, M, loc, method);
  end
end
%=============================================================================
function [Fm, fm] = fillConstant(X, loc, value)
  count = length(loc.miss);
  fm = true(count, 1);
  if iscell(X)
    if ~isText(value)
      error('Nelson:data_analysis:invalidFillConstantTypeTable', 'Invalid fill constant type for table variable.');
    end
    Fm = repmat({char(value)}, count, 1);
  elseif isscalar(value) || ischar(value)
    if isstring(X)
      value = string(value);
    end
    Fm = repmat(value, count, 1);
  else
    % One fill value per slice.
    Fm = reshape(value(loc.col), [], 1);
  end
end
%=============================================================================
function [Fm, fm] = fillNearest(X, x, loc)
  % Ties go to the next nonmissing entry.
  hasPrevious = loc.gpos > loc.lo;
  hasNext = loc.gpos < loc.hi;
  previous = knownIndex(loc, loc.gpos, hasPrevious);
  next = knownIndex(loc, loc.gpos + 1, hasNext);
  xq = x(loc.row);
  usePrevious = hasPrevious & (~hasNext | (xq - x(rowOf(loc, previous))) < (x(rowOf(loc, next)) - xq));
  next(usePrevious) = previous(usePrevious);
  Fm = reshape(X(next), [], 1);
  fm = hasPrevious | hasNext;
end
%=============================================================================
function [Fm, fm] = fillLinear(X, x, loc)
  % Interpolation between the surrounding nonmissing entries, linear
  % extrapolation from the first or last two of them at the ends.
  fm = loc.hi - loc.lo >= 2;
  a = loc.gpos;
  leading = loc.gpos == loc.lo;
  trailing = loc.gpos == loc.hi;
  a(leading) = a(leading) + 1;
  a(trailing) = a(trailing) - 1;
  ia = knownIndex(loc, a, fm);
  ib = knownIndex(loc, a + 1, fm);
  va = reshape(double(X(ia)), [], 1);
  vb = reshape(double(X(ib)), [], 1);
  xa = x(rowOf(loc, ia));
  xb = x(rowOf(loc, ib));
  Fm = va + (vb - va) .* (x(loc.row) - xa) ./ (xb - xa);
end
%=============================================================================
function [Fm, fm] = fillInterp(X, M, x, loc, method)
  count = length(loc.miss);
  Fm = NaN(count, 1);
  fm = false(count, 1);
  [starts, stops] = runBounds([true; diff(loc.col) ~= 0]);
  for s = 1:length(starts)
    r = starts(s):stops(s);
    c = loc.col(starts(s));
    k = find(~M(:, c));
    if length(k) < 2
      continue
    end
    Fm(r) = interp1(x(k), double(X(k, c)), x(loc.row(r)), method, 'extrap');
    fm(r) = true;
  end
end
%=============================================================================
function [starts, stops] = runBounds(breaks)
  starts = find(breaks);
  stops = [starts(2:end) - 1; length(breaks)];
end
%=============================================================================
function [Fm, fm] = fillMoving(X, M, x, loc, method, window, samplePoints)
  % Statistic of the nonmissing entries in the window centered on each missing entry.
  if isempty(samplePoints)
    V = double(X);
    V(M) = NaN;
    if strcmp(method, 'movmean')
      Y = movmean(V, window, 1, 'omitnan');
    else
      Y = movmedian(V, window, 1, 'omitnan');
    end
    Fm = reshape(Y(loc.miss), [], 1);
    fm = ~isnan(Fm);
    return
  end
  % A scalar window w spans [t - w/2, t + w/2), a window [b a] spans [t - b, t + a].
  [before, after] = windowSides(window);
  count = length(loc.miss);
  Fm = NaN(count, 1);
  for k = 1:count
    t = x(loc.row(k));
    if isscalar(window)
      inWindow = x >= t - before & x < t + after;
    else
      inWindow = x >= t - before & x <= t + after;
    end
    values = double(X(inWindow & ~M(:, loc.col(k)), loc.col(k)));
    if ~isempty(values)
      if strcmp(method, 'movmean')
        Fm(k) = mean(values);
      else
        Fm(k) = median(values);
      end
    end
  end
  fm = ~isnan(Fm);
end
%=============================================================================
function [before, after] = windowSides(window)
  if isscalar(window)
    before = window / 2;
    after = before;
  else
    before = window(1);
    after = window(2);
  end
end
%=============================================================================
function [Fm, fm] = fillFunction(X, x, t, loc, fun, window)
  % fun(xs, ts, tq) fills each gap from the entries of the window
  % [t1 - b, t2 + a] around the gap [t1, t2], the gap excluded (possibly
  % none). x: numeric sample points, t: the sample points as given.
  [before, after] = windowSides(window);
  count = length(loc.miss);
  Fm = NaN(count, 1);
  fm = false(count, 1);
  [starts, stops] = runBounds([true; diff(loc.miss) ~= 1 | diff(loc.col) ~= 0]);
  for s = 1:length(starts)
    r = starts(s):stops(s);
    rows = loc.row(r);
    c = loc.col(starts(s));
    sampleIdx = find(x >= x(rows(1)) - before & x <= x(rows(end)) + after);
    sampleIdx = sampleIdx(sampleIdx < rows(1) | sampleIdx > rows(end));
    values = fun(double(X(sampleIdx, c)), t(sampleIdx), t(rows));
    if ~(isscalar(values) || numel(values) == length(rows))
      error('Nelson:data_analysis:fillmissingFunctionOutputSize', 'Output of function handle must have length 1 or must have the length of the missing value gap.');
    end
    Fm(r) = values(:);
    fm(r) = true;
  end
end
%=============================================================================
function [Fm, fm] = fillStatistic(X, M, loc, method)
  stat = sliceStatistic(X, M, method);
  Fm = reshape(stat(loc.col), [], 1);
  fm = ~isnan(Fm);
end
%=============================================================================
function stat = sliceStatistic(X, M, method)
  % Mean, median or mode of the nonmissing values of each slice (column of X).
  if isfloat(X)
    values = X;
  else
    values = double(X);
  end
  values(M) = NaN;
  stat = columnStatistic(values, method);
end
%=============================================================================
function stat = columnStatistic(values, method)
  % values: n x cols with NaN for missing entries. Returns 1 x cols (NaN when a column is all missing).
  [n, cols] = size(values);
  known = ~isnan(values);
  count = sum(known, 1);
  offsets = (0:cols - 1) * n;
  switch method
    case 'mean'
      values(~known) = 0;
      stat = sum(values, 1) ./ count;
    case 'median'
      sorted = sort(values, 1);
      lo = max(floor((count + 1) / 2), 1);
      hi = max(ceil((count + 1) / 2), 1);
      stat = (sorted(lo + offsets) + sorted(hi + offsets)) / 2;
      stat(count == 0) = NaN;
    case 'mode'
      sorted = sort(values, 1);
      valid = ~isnan(sorted);
      starts = valid & [true(1, cols); diff(sorted, 1, 1) ~= 0];
      p = find(starts);
      e = [p(2:end) - 1; numel(sorted)];
      cumulated = cumsum(valid(:));
      runLength = zeros(n, cols);
      runLength(p) = cumulated(e) - cumulated(p) + 1;
      [frequency, idx] = max(runLength, [], 1);
      stat = sorted(idx + offsets);
      stat(frequency == 0) = NaN;
  end
end
%=============================================================================
function [Fm, fm] = applyEndValues(X, Fm, fm, loc, endValues)
  % Leading and trailing missing entries of each slice follow EndValues instead of the fill method.
  leading = loc.gpos == loc.lo;
  trailing = loc.gpos == loc.hi;
  if ischar(endValues)
    method = lower(endValues);
    if ~any(strcmp(method, {'none', 'previous', 'next', 'nearest'}))
      error('Nelson:data_analysis:unsupportedEndValuesMethod', 'Unsupported EndValues method.');
    end
    fm(leading | trailing) = false;
    if any(strcmp(method, {'next', 'nearest'}))
      k = leading & loc.hi > loc.lo;
      Fm(k) = X(loc.p(loc.gpos(k) + 1));
      fm(k) = true;
    end
    if any(strcmp(method, {'previous', 'nearest'}))
      k = trailing & loc.gpos > loc.lo;
      Fm(k) = X(loc.p(loc.gpos(k)));
      fm(k) = true;
    end
  else
    ends = leading | trailing;
    if isscalar(endValues)
      Fm(ends) = endValues;
    else
      Fm(ends) = endValues(loc.col(ends));
    end
    fm(ends) = true;
  end
end
%=============================================================================
function tooLarge = gapTooLarge(x, loc, maxGap)
  % Gap size: distance between the nonmissing values around a gap, or to the first/last sample point for end gaps.
  hasPrevious = loc.gpos > loc.lo;
  hasNext = loc.gpos < loc.hi;
  xl = x(rowOf(loc, knownIndex(loc, loc.gpos, hasPrevious)));
  xr = x(rowOf(loc, knownIndex(loc, loc.gpos + 1, hasNext)));
  xl(~hasPrevious) = x(1);
  xr(~hasNext) = x(end);
  tooLarge = (xr - xl) > maxGap;
end
%=============================================================================
