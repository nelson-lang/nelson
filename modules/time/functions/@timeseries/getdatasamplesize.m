%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sz = getdatasamplesize(obj)
  if isempty(obj.Data)
    sz = [0 0];
    return
  end
  sz = size(obj.Data);
  dim = timeseriesSampleDimension(obj.Data, obj.IsTimeFirst);
  sz(dim) = [];
  if isempty(sz)
    sz = [1 1];
  elseif isscalar(sz)
    sz = [1 sz];
  end
end
%=============================================================================
