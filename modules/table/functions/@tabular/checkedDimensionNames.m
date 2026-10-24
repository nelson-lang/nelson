%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = checkedDimensionNames(obj, names)
  % Dimension names must be two distinct nonempty names given as a string
  % array or a cell array of character vectors; they are returned as a row
  % cell.
  if isstring(names)
    names = cellstr(names);
  end
  if ~iscellstr(names) || any(cellfun('isempty', names(:)))
    error('Nelson:table:invalidDimensionNames', _('The DimensionNames property must be a string array or a cell array, with each name containing one or more characters.'));
  end
  if numel(names) ~= 2
    error('Nelson:table:invalidDimensionNamesCount', _('The DimensionNames property must contain one name for each dimension in the table.'));
  end
  if strcmp(names{1}, names{2})
    error('Nelson:table:duplicateDimensionNames', _('Duplicate table dimension name: ''%s''.'), names{1});
  end
  names = names(:)';
end
%=============================================================================
