%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function dim = timeseriesSampleDimension(data, isTimeFirst)
  if isempty(data)
    dim = 1;
  elseif isvector(data)
    if isrow(data)
      dim = 2;
    else
      dim = 1;
    end
  elseif isTimeFirst
    dim = 1;
  else
    dim = ndims(data);
  end
end
%=============================================================================
