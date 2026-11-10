%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function idx = subsindex(a)
  % zero-based positions of a device index used on a host array; a mask gives its true positions
  v = gather(a);
  if islogical(v)
    v = find(v);
  end
  idx = double(v) - 1;
end
%=============================================================================
