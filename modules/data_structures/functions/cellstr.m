%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ce = cellstr(obj)
  %{## Converts to cell of character array.
  ### Syntax
      ce = cellstr(A)
  ### Input argument
  A
  
         a string, a string array, cell of character array.
  
  ### Output argument
  ce
  
         a cell of character array.
  ### Description
  <b>cellstr(A)</b> converts to cell of character array.
  ### Example
  <pre>
  cellstr('Nelson')
  cellstr({'Nelson'})
  cellstr({})
  </pre>
  %}

  if iscellstr(obj)
    ce = obj;
  elseif isstring(obj)
    if ~any(ismissing(obj))
      ce = cell(obj);
    else
      error(_('Cannot convert missing element.'));
    end
  elseif iscell(obj)
    error(_('Cell must be string scalars or character arrays.'));
  elseif ischar(obj)
    if isempty(obj)
      ce = {''};
    elseif ndims(size(obj)) != 2
      error(_('Matrix 2D expected.'));
    else
      numrows = size(obj,1);
      ce = cell(numrows,1);
      for i = 1:numrows
        ce{i} = obj(i, :);
      end
      ce = deblank(ce);
    end
  else
    error(_(['Type not supported:' ,' ', class(obj)]));
  end
end
%=============================================================================
