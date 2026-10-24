%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, TFrm, TFoutlier, L, U, C] = rmoutliers(A, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 6);
  if ~(isnumeric(A) && isreal(A) && ~issparse(A) && ismatrix(A))
    error('Nelson:statistics:realNumericVectorMatrix', '%s must be a real numeric vector or matrix.', 'A');
  end
  [leadingArgs, nameValueArgs, removeDim, locations] = parseInputsLocal(A, convertStringToCharArgs(varargin));
  detectionDim = detectionDimensionLocal(A);
  if isempty(locations)
    [TFoutlier, L, U, C] = isoutlier(A, leadingArgs{:}, detectionDim, nameValueArgs{:});
  else
    TFoutlier = locations;
    [L, U, C] = emptyThresholdsLocal(size(A), detectionDim);
  end
  TFrm = removedEntriesLocal(TFoutlier, removeDim);
  B = removeEntriesLocal(A, TFrm, removeDim);
end
%=============================================================================
function [leadingArgs, nameValueArgs, removeDim, locations] = parseInputsLocal(A, args)
  leadingArgs = {};
  nameValueArgs = {};
  removeDim = defaultRemoveDimensionLocal(A);
  locations = [];
  idx = 1;
  if idx <= length(args)
    value = args{idx};
    if isTextScalarLocal(value) && ~isLeadingNameValueLocal(value)
      method = parseMethodNameLocal(value);
      leadingArgs{end + 1} = value;
      idx = idx + 1;
      if any(strcmp(method, {'percentiles', 'movmedian', 'movmean'}))
        if idx > length(args)
          error('Nelson:statistics:methodArgumentIncomplete', 'Method argument is incomplete.');
        end
        leadingArgs{end + 1} = args{idx};
        idx = idx + 1;
      end
    elseif isRemoveDimLocal(value)
      removeDim = double(value);
      idx = idx + 1;
    end
  end
  if idx <= length(args) && isRemoveDimLocal(args{idx})
    removeDim = double(args{idx});
    idx = idx + 1;
  end
  if rem(length(args) - idx + 1, 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  while idx <= length(args)
    name = matchedNameLocal(args{idx});
    if strcmp(name, 'outlierlocations')
      locations = parseOutlierLocationsLocal(args{idx + 1}, size(A));
    else
      nameValueArgs{end + 1} = args{idx};
      nameValueArgs{end + 1} = args{idx + 1};
    end
    idx = idx + 2;
  end
end
%=============================================================================
function TFrm = removedEntriesLocal(TFoutlier, removeDim)
  if isvector(TFoutlier)
    TFrm = TFoutlier;
    return
  end
  if removeDim == 1
    TFrm = (double(TFoutlier) * ones(size(TFoutlier, 2), 1)) > 0;
  else
    TFrm = (ones(1, size(TFoutlier, 1)) * double(TFoutlier)) > 0;
  end
end
%=============================================================================
function B = removeEntriesLocal(A, TFrm, removeDim)
  if isvector(A)
    if size(A, 1) == 1
      B = A(:, ~TFrm);
    else
      B = A(~TFrm, :);
    end
  elseif removeDim == 1
    B = A(~TFrm, :);
  else
    B = A(:, ~TFrm);
  end
end
%=============================================================================
function [L, U, C] = emptyThresholdsLocal(sz, detectionDim)
  outSize = sz;
  outSize(detectionDim) = 1;
  L = NaN(outSize);
  U = NaN(outSize);
  C = NaN(outSize);
end
%=============================================================================
function locations = parseOutlierLocationsLocal(value, sz)
  if ~(islogical(value) && isequal(size(value), sz))
    error('Nelson:statistics:outlierLocationsLogicalArraySameSize', 'OutlierLocations must be a logical array the same size as A.');
  end
  locations = value;
end
%=============================================================================
function dim = defaultRemoveDimensionLocal(A)
  if isvector(A)
    dim = detectionDimensionLocal(A);
  else
    dim = 1;
  end
end
%=============================================================================
function dim = detectionDimensionLocal(A)
  if size(A, 1) ~= 1
    dim = 1;
  else
    dim = 2;
  end
end
%=============================================================================
function method = parseMethodNameLocal(value)
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
function tf = isRemoveDimLocal(value)
  tf = isnumeric(value) && isreal(value) && isscalar(value) && isfinite(value) ...
    && (value == 1 || value == 2);
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================
