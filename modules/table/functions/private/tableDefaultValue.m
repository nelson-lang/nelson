%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = tableDefaultValue(sample, nRows)
  if nargin < 2
    nRows = 1;
  end
  sz = size(sample);
  sz(1) = nRows;
  if isnumeric(sample)
    if isfloat(sample)
      value = NaN(sz);
      value = cast(value, class(sample));
    else
      value = zeros(sz, class(sample));
    end
  elseif islogical(sample)
    value = false(sz);
  elseif isstring(sample)
    value = strings(sz);
  elseif iscell(sample)
    value = cell(sz);
  else
    value = cell(nRows, 1);
  end
end
%=============================================================================
