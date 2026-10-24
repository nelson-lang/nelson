%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = checkedVariableNames(obj, names)
  % Variable names given as a whole must be a string array or a cell array
  % of character vectors; they are returned as a row cell.
  if ~iscell(names) && ~isstring(names)
    error('Nelson:table:invalidVariableNames', _('To assign multiple variable names, specify nonempty names in a string array or a cell array of character vectors.'));
  end
  names = toCellstrRow(obj, names);
end
%=============================================================================
