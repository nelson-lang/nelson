%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [sub, newTimes] = timetableVariableRowSubscript(props, field, sub, allowNew)
  % Subscripts following TT.name or TT.(n) (variable number): row times, a
  % timerange, a withtol or text as the first parenthesis subscript of a
  % variable are converted to row numbers (newTimes: the new row times of
  % an assignment).
  newTimes = props.RowTimes([]);
  if isempty(sub)
    return
  end
  if isnumeric(field) && isscalar(field) && any(field == 1:numel(props.VariableNames))
    field = props.VariableNames{field};
  end
  first = sub(1);
  if ~strcmp(first.type, '()') || isempty(first.subs) || ~any(strcmp(field, props.VariableNames))
    return
  end
  rows = first.subs{1};
  isTime = isa(rows, 'datetime') || isa(rows, 'duration') || isa(rows, 'timerange') || isa(rows, 'withtol');
  isText = isstring(rows) || iscellstr(rows) || (ischar(rows) && ~strcmp(rows, ':'));
  if isTime || isText
    [sub(1).subs{1}, newTimes] = timetableResolveRows(props.RowTimes, rows, nargin > 3 && allowNew);
  end
end
%=============================================================================
