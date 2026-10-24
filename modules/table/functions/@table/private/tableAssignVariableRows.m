%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function col = tableAssignVariableRows(col, rows, data, name)
  % T(rows, vars) = value: a variable of value replaces the indexed rows of
  % the variable name; it must have their size, or be a scalar.
  leftSize = size(col);
  leftSize(1) = numel(rows);
  isScalarData = isscalar(data); % not inlined: an if on isscalar(object) ignores its overload
  if ~isScalarData && ~isequal(size(data), leftSize)
    error('Nelson:table:AssignmentDimensionMismatch', 'Unable to perform assignment for table variable ''%s'' because its size on the left side is %s and the size of the corresponding variable on the right side is %s.', ...
      name, sizeText(leftSize), sizeText(size(data)));
  end
  col(rows, :) = data;
end
%=============================================================================
function txt = sizeText(sz)
  txt = char(strjoin(string(sz), '-by-'));
end
%=============================================================================
