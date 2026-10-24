%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ic] = unique(TT, varargin)
  % Row times are the first key, then the variables: the table unique does
  % the work on a table whose first variable holds the row times.
  st = struct(TT);
  tableObject = table.fromStruct(st.Table);
  names = tableObject.Properties.VariableNames;
  timeName = 'Time';
  while any(strcmp(timeName, names))
    timeName = [timeName, '_'];
  end
  timeTable = table(st.Properties.RowTimes(:), 'VariableNames', {timeName});
  [~, ia, ic] = unique([timeTable, tableObject], varargin{:});
  C = TT(ia, :);
end
%=============================================================================
