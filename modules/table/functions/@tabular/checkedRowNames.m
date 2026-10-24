%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = checkedRowNames(obj, names)
  % Row names must be a string array or a cell array of nonempty character
  % vectors; they are returned as a column cell.
  if isstring(names)
    names = cellstr(names);
  end
  if ~iscellstr(names) || any(cellfun('isempty', names(:)))
    error('Nelson:table:invalidRowNames', _('The RowNames property must be a string array or a cell array, with each name containing one or more characters.'));
  end
  if isempty(names)
    names = {};
  else
    names = names(:);
  end
end
%=============================================================================
