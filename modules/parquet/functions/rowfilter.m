%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filter = rowfilter(source)
  if nargin == 0
    names = {};
  elseif istable(source) || istimetable(source)
    names = source.Properties.VariableNames;
  elseif isa(source, 'nelson.io.parquet.ParquetInfo')
    names = cellstr(source.VariableNames);
  elseif iscell(source) || isstring(source) || ischar(source)
    names = parquetRowFilterNames(source);
  else
    names = source.VariableNames;
  end
  filter = nelson.io.RowFilter(names);
end
%=============================================================================
function names = parquetRowFilterNames(source)
  if ischar(source)
    names = {source};
  elseif isstring(source)
    names = cellstr(source);
  else
    names = source;
  end
end
%=============================================================================
