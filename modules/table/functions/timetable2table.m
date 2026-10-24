%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function T = timetable2table(TT, varargin)
  narginchk(1, Inf);
  if ~istimetable(TT)
    error('Nelson:table:invalidType:valueATimetable', '%s must be a timetable.', 'TT');
  end
  opts = parseOptions(varargin{:});
  st = struct(TT);
  T = table.fromStruct(st.Table);
  if opts.ConvertRowTimes
    rowTimesName = st.Properties.DimensionNames{1};
    if isempty(rowTimesName)
      rowTimesName = 'Time';
    end
    rowTimesName = uniqueNameLocal(rowTimesName, st.Table.Properties.VariableNames);
    T = table(st.Properties.RowTimes, 'VariableNames', {rowTimesName});
    T = [T, table.fromStruct(st.Table)];
  end
end
%=============================================================================
function opts = parseOptions(varargin)
  opts.ConvertRowTimes = true;
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
      case 'convertrowtimes'
        opts.ConvertRowTimes = logical(varargin{k + 1});
      otherwise
        error('Nelson:table:unsupportedOption', 'Unsupported "%s" option.', 'timetable2table');
    end
    k = k + 2;
  end
end
%=============================================================================
function name = uniqueNameLocal(name, existing)
  if ~any(strcmp(existing, name))
    return
  end
  base = name;
  k = 1;
  while any(strcmp(existing, name))
    name = [base, '_', int2str(k)];
    k = k + 1;
  end
end
%=============================================================================
