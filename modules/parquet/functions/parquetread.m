%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = parquetread(filename, varargin)
  opts = parquetParseReadOptions(varargin{:});
  expression = [];
  if ~isempty(opts.RowFilter)
    expression = opts.RowFilter.Expression;
  end
  T = _parquetread(filename, opts.SelectedVariableNames, opts.RowGroups, expression);
  if ~isempty(opts.RowFilter)
    T = opts.RowFilter.apply(T);
  end
  if strcmpi(opts.OutputType, 'timetable')
    T = parquetMakeTimetable(T, opts);
  elseif ~strcmpi(opts.OutputType, 'table')
    error('Nelson:parquetread:invalidOutputType', 'OutputType must be ''table'' or ''timetable''.');
  end
end
%=============================================================================
function opts = parquetParseReadOptions(varargin)
  opts.OutputType = 'table';
  opts.SelectedVariableNames = {};
  opts.RowTimes = [];
  opts.StartTime = [];
  opts.SampleRate = [];
  opts.TimeStep = [];
  opts.RowGroups = [];
  opts.RowFilter = [];
  opts.VariableNamingRule = 'modify';
  if mod(nargin, 2) ~= 0
    error('Nelson:parquet:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:nargin
    name = char(varargin{k});
    value = varargin{k + 1};
    switch lower(name)
      case 'outputtype'
        opts.OutputType = char(value);
      case 'selectedvariablenames'
        opts.SelectedVariableNames = parquetStringList(value);
      case 'rowtimes'
        opts.RowTimes = value;
      case 'starttime'
        opts.StartTime = value;
      case 'samplerate'
        opts.SampleRate = value;
      case 'timestep'
        opts.TimeStep = value;
      case 'rowgroups'
        opts.RowGroups = value;
      case 'rowfilter'
        opts.RowFilter = value;
      case 'variablenamingrule'
        opts.VariableNamingRule = char(value);
      otherwise
        error('Nelson:parquetread:unsupportedOption', 'Unsupported parquetread option.');
    end
  end
end
%=============================================================================
function names = parquetStringList(value)
  if isempty(value)
    names = {};
  elseif ischar(value)
    names = {value};
  elseif isstring(value)
    names = cellstr(value);
  else
    names = value;
  end
end
%=============================================================================
function TT = parquetMakeTimetable(T, opts)
  if ~isempty(opts.RowTimes)
    rowTimes = opts.RowTimes;
  else
    n = size(T, 1);
    if ~isempty(opts.StartTime) && ~isempty(opts.SampleRate)
      rowTimes = opts.StartTime + seconds((0:n - 1)' ./ opts.SampleRate);
    elseif ~isempty(opts.StartTime) && ~isempty(opts.TimeStep)
      rowTimes = opts.StartTime + (0:n - 1)' .* opts.TimeStep;
    elseif ~isempty(T.Properties.VariableNames)
      rowTimes = T.(T.Properties.VariableNames{1});
      T = T(:, 2:end);
    else
      error('Nelson:parquetread:rowTimesRequired', 'Row times are required to create a timetable.');
    end
  end
  TT = timetable(rowTimes, T);
end
%=============================================================================
