%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = timeseriesLengthFromData(data, isTimeFirst)
  if isempty(data)
    n = 0;
    return
  end
  if isvector(data)
    n = numel(data);
  elseif isTimeFirst
    n = size(data, 1);
  else
    n = size(data, ndims(data));
  end
end
%=============================================================================
