%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = table2timetable(T, varargin)
  narginchk(1, Inf);
  if ~istable(T)
    error('Nelson:table:invalidType', '%s must be a table.', 'T');
  end
  opts = parseOptions(varargin{:});
  st = struct(T);
  if ~isempty(opts.RowTimes)
    rowTimes = opts.RowTimes;
    dataTable = T;
  else
    timeVar = resolveTimeVariable(st, opts.TimeVariable);
    if isempty(timeVar)
      error('Nelson:table:invalidSyntax:aTimeVariableRowTimesSpecified', 'A time variable or RowTimes must be specified.');
    end
    timeName = st.Properties.VariableNames{timeVar};
    rowTimes = st.data.(timeName);
    dataTable = T;
    dataTable(:, timeVar) = [];
  end
  props = [];
  if exist('timeName', 'var')
    props = struct('DimensionNames', {{timeName, 'Variables'}});
  end
  TT = timetable.fromTableAndTimes(dataTable, rowTimes, props);
end
%=============================================================================
function opts = parseOptions(varargin)
  opts.RowTimes = [];
  opts.TimeVariable = [];
  k = 1;
  while k <= nargin
    name = varargin{k};
    if isstring(name)
      name = char(name);
    end
    if ~ischar(name) || k == nargin
      error('Nelson:table:nameValuePair', 'Name-value argument expected.');
    end
    switch lower(name)
      case 'rowtimes'
        opts.RowTimes = varargin{k + 1};
      case 'timevariable'
        opts.TimeVariable = varargin{k + 1};
      otherwise
        error('Nelson:table:unsupportedOption', 'Unsupported "%s" option.', 'table2timetable');
    end
    k = k + 2;
  end
end
%=============================================================================
function idx = resolveTimeVariable(st, selector)
  idx = [];
  if isempty(selector)
    for k = 1:length(st.Properties.VariableNames)
      value = st.data.(st.Properties.VariableNames{k});
      if isa(value, 'datetime') || isa(value, 'duration')
        idx = k;
        return
      end
    end
    return
  end
  if isnumeric(selector)
    idx = selector;
  elseif isstring(selector)
    selector = char(selector);
    idx = find(strcmp(st.Properties.VariableNames, selector), 1);
  elseif ischar(selector)
    idx = find(strcmp(st.Properties.VariableNames, selector), 1);
  end
  if isempty(idx)
    error('Nelson:table:unrecognizedTimeVariable', 'Unrecognized time variable.');
  end
end
%=============================================================================
