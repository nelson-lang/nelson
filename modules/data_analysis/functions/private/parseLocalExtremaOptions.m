%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function opts = parseLocalExtremaOptions(A, varargin)
  % Validates the arguments of islocalmax and islocalmin.
  isTabular = istable(A) || istimetable(A);
  if ~isTabular
    if ~(isnumeric(A) || islogical(A))
      error('Nelson:isLocalExtrema:FirstInputInvalid', 'First argument must be a numeric or logical array, a table, or a timetable.');
    end
    if ~isreal(A)
      error('Nelson:isLocalExtrema:ComplexInputArray', 'Complex input arguments are not supported.');
    end
  end
  opts = struct('dim', 1, 'MaxNumExtrema', Inf, 'MinSeparation', 0, ...
    'MinProminence', 0, 'FlatSelection', 'center', 'SamplePoints', [], ...
    'ProminenceWindow', [], 'DataVariables', [], 'OutputFormat', 'logical', ...
    'hasSamplePoints', false, 'hasMinSeparation', false, 'isTabular', isTabular);
  if ~isTabular
    opts.dim = find(size(A) ~= 1, 1);
    if isempty(opts.dim)
      opts.dim = 1;
    end
  end
  first = 1;
  if ~isempty(varargin) && (isnumeric(varargin{1}) || islogical(varargin{1}))
    if isTabular
      error('Nelson:isLocalExtrema:DimensionTable', 'Dimension is not supported when the first argument is a table or timetable.');
    end
    opts.dim = parseDimension(varargin{1});
    first = 2;
  end
  if mod(numel(varargin) - first + 1, 2) ~= 0
    error('Nelson:isLocalExtrema:NameValuePairs', 'Incorrect number of input arguments. Parameter names and values must occur in pairs.');
  end
  names = {'MaxNumExtrema', 'MinSeparation', 'MinProminence', 'FlatSelection', ...
    'SamplePoints', 'ProminenceWindow', 'DataVariables', 'OutputFormat'};
  for k = first:2:numel(varargin)
    index = matchOptionName(varargin{k}, names);
    if isempty(index)
      invalidName(isTabular);
    end
    opts = parseOption(opts, names{index}, varargin{k + 1}, A);
  end
end
%=============================================================================
function opts = parseOption(opts, name, value, A)
  switch name
    case 'MaxNumExtrema'
      if ~(isnumeric(value) || islogical(value)) || ~isscalar(value) || ~isreal(value) || ~(value > 0) || (fix(value) ~= value)
        error('Nelson:isLocalExtrema:MaxNumInvalid', '''MaxNumExtrema'' value must be a positive integer.');
      end
      opts.MaxNumExtrema = double(value);
    case 'MinSeparation'
      if ~isNonnegativeFiniteScalar(value, true)
        error('Nelson:isLocalExtrema:MinSeparationInvalid', '''MinSeparation'' value must be a nonnegative, real, finite scalar.');
      end
      opts.MinSeparation = value;
      opts.hasMinSeparation = true;
    case 'MinProminence'
      if ~(isnumeric(value) || islogical(value)) || ~isNonnegativeFiniteScalar(value, false)
        error('Nelson:isLocalExtrema:MinProminenceInvalid', '''MinProminence'' value must be a nonnegative numeric scalar.');
      end
      opts.MinProminence = double(value);
    case 'FlatSelection'
      flatOptions = {'all', 'first', 'center', 'last'};
      index = matchOptionName(value, flatOptions);
      if isempty(index)
        error('Nelson:isLocalExtrema:FlatSelectionInvalid', '''FlatSelection'' must be ''first'', ''center'', ''last'', or ''all''.');
      end
      opts.FlatSelection = flatOptions{index};
    case 'SamplePoints'
      if istimetable(A)
        error('Nelson:isLocalExtrema:SamplePointsTimeTable', '''SamplePoints'' parameter not supported when first argument is a timetable.');
      end
      opts.SamplePoints = value;
      opts.hasSamplePoints = true;
    case 'ProminenceWindow'
      if ~isValidProminenceWindow(value)
        error('Nelson:isLocalExtrema:ProminenceWindowLengthInvalid', '''ProminenceWindow'' value must be a positive scalar or 2-element vector of nonnegative scalars.');
      end
      opts.ProminenceWindow = value;
    case 'DataVariables'
      if ~opts.isTabular
        error('Nelson:isLocalExtrema:DataVariablesArray', '''DataVariables'' parameter is only supported when the first input argument is a table or timetable.');
      end
      opts.DataVariables = value;
    otherwise
      if ~opts.isTabular
        error('Nelson:isLocalExtrema:OutputFormatArray', '''OutputFormat'' parameter is only supported when the first input argument is a table or timetable.');
      end
      formats = {'logical', 'tabular'};
      index = matchOptionName(value, formats);
      if isempty(index)
        error('Nelson:isLocalExtrema:OutputFormatInvalid', '''OutputFormat'' value must be ''logical'' or ''tabular''.');
      end
      opts.OutputFormat = formats{index};
  end
end
%=============================================================================
function dim = parseDimension(value)
  if ~isscalar(value) || ~isreal(value) || ~(value > 0) || ~isfinite(value) || (fix(value) ~= value)
    error('Nelson:isLocalExtrema:DimensionInvalid', 'Dimension must be a positive integer.');
  end
  dim = double(value);
end
%=============================================================================
function index = matchOptionName(value, names)
  % unique case-insensitive prefix match, [] otherwise.
  index = [];
  if ~((ischar(value) && (isrow(value) || isempty(value))) || (isstring(value) && isscalar(value)))
    return
  end
  value = char(value);
  if isempty(value)
    return
  end
  matches = find(strncmpi(value, names, length(value)));
  if isscalar(matches)
    index = matches;
  end
end
%=============================================================================
function invalidName(isTabular)
  if isTabular
    error('Nelson:isLocalExtrema:InvalidNameTable', 'Parameter name must be ''MaxNumExtrema'', ''MinSeparation'', ''MinProminence'', ''FlatSelection'', ''SamplePoints'', ''ProminenceWindow'', ''DataVariables'', or ''OutputFormat''.');
  end
  error('Nelson:isLocalExtrema:InvalidNameArray', 'Parameter name must be ''MaxNumExtrema'', ''MinSeparation'', ''MinProminence'', ''FlatSelection'', ''SamplePoints'', or ''ProminenceWindow''.');
end
%=============================================================================
function tf = isNonnegativeFiniteScalar(value, allowDuration)
  if allowDuration && isduration(value)
    tf = isscalar(value) && (milliseconds(value) >= 0) && isfinite(milliseconds(value));
    return
  end
  tf = (isnumeric(value) || islogical(value)) && isscalar(value) && isreal(value) && (value >= 0) && isfinite(value);
end
%=============================================================================
function tf = isValidProminenceWindow(value)
  if isduration(value)
    value = milliseconds(value);
  elseif ~(isnumeric(value) && isreal(value))
    tf = false;
    return
  end
  if isscalar(value)
    tf = ~isnan(value) && (value > 0);
  else
    tf = isvector(value) && (numel(value) == 2) && all(~isnan(value)) && all(value >= 0);
  end
end
%=============================================================================
