%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tableCheckRowNamesCount(props, data)
  % Nonempty (already validated) RowNames name every row. The height is
  % read from the data of the first variable; without variables, from the
  % n-by-0 data struct (a scalar field-less struct is an unset height, as
  % in table()).
  nNames = numel(props.RowNames);
  if nNames == 0
    return
  end
  names = props.VariableNames;
  if isempty(names)
    if ~isempty(data)
      return
    end
    nRows = size(data, 1);
  elseif iscell(names) && isfield(data, names{1})
    nRows = size(data.(names{1}), 1);
  else
    return
  end
  if nNames ~= nRows
    error('Nelson:table:incorrectNumberOfRowNames', _('The RowNames property must contain one name for each row in the table.'));
  end
end
%=============================================================================
