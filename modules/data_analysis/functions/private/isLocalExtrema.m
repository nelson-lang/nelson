%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [TF, P] = isLocalExtrema(A, isMaximum, wantProminence, varargin)
  % Shared implementation of islocalmax and islocalmin.
  opts = parseLocalExtremaOptions(A, varargin{:});
  if opts.isTabular
    [TF, P] = tabularExtrema(A, isMaximum, wantProminence, opts);
  else
    [TF, P] = arrayExtrema(A, isMaximum, wantProminence, opts);
  end
end
%=============================================================================
function [TF, P] = arrayExtrema(A, isMaximum, wantProminence, opts)
  [x, minSeparation, window] = resolveSamplePoints(opts, opts.SamplePoints, size(A, opts.dim), false);
  isSparse = issparse(A);
  if isSparse
    A = full(A);
  end
  [TF, P] = callBuiltin(A, isMaximum, wantProminence, opts, x, minSeparation, window, opts.dim);
  if isSparse
    TF = sparse(TF);
    if wantProminence
      P = sparse(P);
    end
  end
end
%=============================================================================
function [TF, P] = callBuiltin(A, isMaximum, wantProminence, opts, x, minSeparation, window, dim)
  P = [];
  if wantProminence
    [TF, P] = __islocalextrema__(A, isMaximum, dim, opts.FlatSelection, opts.MinProminence, minSeparation, opts.MaxNumExtrema, x, window);
  else
    TF = __islocalextrema__(A, isMaximum, dim, opts.FlatSelection, opts.MinProminence, minSeparation, opts.MaxNumExtrema, x, window);
  end
end
%=============================================================================
function [TF, P] = tabularExtrema(A, isMaximum, wantProminence, opts)
  names = A.Properties.VariableNames;
  stA = struct(A);
  dataVars = 1:width(A);
  if ~isempty(opts.DataVariables)
    dataVars = tableResolveVariables(stA.Properties.VariableNames, opts.DataVariables, stA.Properties.VariableTypes, stA.data);
  end
  samplePoints = opts.SamplePoints;
  if istimetable(A)
    samplePoints = A.Properties.RowTimes;
  elseif opts.hasSamplePoints && isTableVariableReference(samplePoints, height(A))
    spVar = tableResolveVariables(names, samplePoints);
    samplePoints = A.(names{spVar});
    dataVars(dataVars == spVar) = [];
  end
  [x, minSeparation, window] = resolveSamplePoints(opts, samplePoints, height(A), istimetable(A));
  for v = dataVars
    checkTableVariable(A.(names{v}));
  end
  tabular = strcmp(opts.OutputFormat, 'tabular');
  if tabular
    TF = A(:, dataVars);
  else
    TF = false(height(A), width(A));
  end
  P = [];
  if wantProminence
    P = A(:, dataVars);
  end
  for v = dataVars
    [tfv, pv] = callBuiltin(A.(names{v}), isMaximum, wantProminence, opts, x, minSeparation, window, 1);
    if tabular
      TF.(names{v}) = tfv;
    else
      TF(:, v) = tfv;
    end
    if wantProminence
      P.(names{v}) = pv;
    end
  end
end
%=============================================================================
function tf = isTableVariableReference(value, nRows)
  tf = ischar(value) || isstring(value) || iscellstr(value) || isa(value, 'function_handle') || strcmp(class(value), 'vartype') || islogical(value) || (isnumeric(value) && isscalar(value) && nRows ~= 1);
end
%=============================================================================
function checkTableVariable(value)
  if ~(isnumeric(value) || islogical(value))
    error('Nelson:isLocalExtrema:NonNumericTableVar', 'Table variables must be numeric.');
  end
  if ~isreal(value)
    error('Nelson:isLocalExtrema:ComplexInputArray', 'Complex input arguments are not supported.');
  end
  if ~(iscolumn(value) || (isempty(value) && ismatrix(value) && all(size(value) <= [0 1])))
    error('Nelson:isLocalExtrema:NonVectorTableVariable', 'Table variables must be column vectors.');
  end
end
%=============================================================================
function [x, minSeparation, window] = resolveSamplePoints(opts, samplePoints, n, isTimetable)
  % Numeric sample points (milliseconds for times), separation and window in
  % the same units; separation and window are dropped when they cannot act.
  minSeparation = opts.MinSeparation;
  window = opts.ProminenceWindow;
  isTime = isdatetime(samplePoints) || isduration(samplePoints);
  if isTime
    if ~isduration(minSeparation)
      if opts.hasMinSeparation
        if isTimetable
          error('Nelson:isLocalExtrema:SeparationMustBeDurationTimetable', '''MinSeparation'' value must be a duration scalar when the first input argument is a timetable.');
        end
        error('Nelson:isLocalExtrema:SeparationMustBeDuration', '''MinSeparation'' value must be a duration scalar if the ''SamplePoints'' values are datetimes or durations.');
      end
      minSeparation = 0;
    else
      minSeparation = milliseconds(minSeparation);
    end
    if ~isempty(window)
      if ~isduration(window)
        if isTimetable
          error('Nelson:isLocalExtrema:ProminenceWindowMustBeDurationTimetable', '''ProminenceWindow'' value must be a duration when the first input argument is a timetable.');
        end
        error('Nelson:isLocalExtrema:ProminenceWindowMustBeDuration', '''ProminenceWindow'' value must be a duration if the ''SamplePoints'' values are datetimes or durations.');
      end
      window = milliseconds(window);
    end
  else
    if isduration(minSeparation)
      error('Nelson:isLocalExtrema:SeparationCannotBeDuration', '''MinSeparation'' value must be a numeric scalar if the ''SamplePoints'' values are numeric.');
    end
    if isduration(window)
      error('Nelson:isLocalExtrema:ProminenceWindowCannotBeDuration', '''ProminenceWindow'' value must be numeric if the ''SamplePoints'' values are numeric.');
    end
  end
  x = checkSamplePoints(samplePoints, n, isTime);
  minSeparation = double(minSeparation);
  window = double(window);
  if isempty(x)
    bounds = [1, n];
  else
    bounds = [x(1), x(end)];
  end
  if ~isempty(window) && all(window ./ (1 + isscalar(window)) > (bounds(2) - bounds(1)))
    window = [];
  end
  minimumDistance = 1;
  if numel(x) >= 2
    minimumDistance = min(diff(x));
  end
  if minSeparation < minimumDistance
    minSeparation = 0;
  end
  if (minSeparation == 0) && isempty(window)
    x = [];
  end
end
%=============================================================================
function x = checkSamplePoints(samplePoints, n, isTime)
  x = [];
  if isempty(samplePoints) && ~isTime
    return
  end
  if ~(isTime || isfloat(samplePoints)) || issparse(samplePoints) || ~(isvector(samplePoints) || isempty(samplePoints))
    error('Nelson:isLocalExtrema:SamplePointsInvalidDatatype', '''SamplePoints'' value must be a vector of type double, single, duration, or datetime.');
  end
  if numel(samplePoints) ~= n
    error('Nelson:isLocalExtrema:SamplePointsLength', 'The sample points vector length must be %d, the same as the size of the input data in the operating dimension.', n);
  end
  if n == 0
    return
  end
  if isTime
    x = milliseconds(samplePoints - samplePoints(1));
  else
    if ~isreal(samplePoints)
      error('Nelson:isLocalExtrema:SamplePointsComplex', '''SamplePoints'' value must be real.');
    end
    x = double(samplePoints);
  end
  x = reshape(x, 1, []);
  if ~all(isfinite(x))
    error('Nelson:isLocalExtrema:SamplePointsNonFinite', '''SamplePoints'' value must not contain Inf or NaN.');
  end
  steps = diff(x);
  if any(steps < 0)
    error('Nelson:isLocalExtrema:SamplePointsSorted', '''SamplePoints'' value must be sorted in ascending order.');
  end
  if any(steps == 0)
    error('Nelson:isLocalExtrema:SamplePointsDuplicate', '''SamplePoints'' value contains duplicates.');
  end
end
%=============================================================================
