%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = tableParenAssignedValue(value, rows, varSub, variableNames)
  % T(rows, vars) = value: returns value as a table with one row per indexed
  % row and one variable per indexed variable. A cell array is converted, a
  % 1x1 table (or cell) is expanded like a scalar. Any other array is sized
  % like a table (rows, then columns) and refused.
  nVars = tableSubscriptCount(varSub, variableNames);
  if iscell(value)
    value = cell2table(value);
  elseif ~istable(value)
    isScalarValue = isscalar(value); % not inlined: an if on isscalar(object) ignores its overload
    if ~isScalarValue
      checkSize(size(value, 1), size(value, 2), numel(rows), nVars);
    end
    error('Nelson:table:invalidRHS', 'Right hand side of an assignment into a table must be another table or a cell array.');
  end
  if height(value) == 1 && width(value) == 1 && nVars > 0
    data = value.(value.Properties.VariableNames{1});
    trailing = repmat({':'}, 1, ndims(data) - 1);
    column = data(ones(numel(rows), 1), trailing{:}); % indexing: any class
    columns = repmat({column}, 1, nVars);
    value = table(columns{:});
  end
  checkSize(height(value), width(value), numel(rows), nVars);
end
%=============================================================================
function checkSize(valueRows, valueVars, nRows, nVars)
  if valueRows ~= nRows
    error('Nelson:table:RowDimensionMismatch', 'To assign to or create a variable in a table, the number of rows must match the height of the table.');
  end
  if valueVars ~= nVars
    error('Nelson:table:VarDimensionMismatch', 'The number of table variables in an assignment must match.');
  end
end
%=============================================================================
