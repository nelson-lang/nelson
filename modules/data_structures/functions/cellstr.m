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
    ce = cell(obj);
    % <missing> becomes empty text
    ce(ismissing(obj)) = {''};
  elseif iscell(obj)
    error('Nelson:data_structures:cellStringScalarsCharacterArrays', 'Cell must be string scalars or character arrays.');
  elseif ischar(obj)
    if isempty(obj)
      ce = {''};
    elseif ndims(obj) != 2
      error('Nelson:validators:mustBeMatrix', '%s must be a 2-D matrix.', 'Value');
    else
      numrows = size(obj, 1);
      ce = cell(numrows, 1);
      for i = 1:numrows
        ce{i} = obj(i, :);
      end
      ce = deblank(ce);
    end
  else
    error('Nelson:cellstr:typeSupported2', 'Type not supported: %s', class(obj));
  end
end
%=============================================================================
