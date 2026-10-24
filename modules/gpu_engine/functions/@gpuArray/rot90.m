%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = rot90(a, k)
  % rotate 90 degrees counter-clockwise, k times; composed from the device
  % transpose and flip overloads (no host transfer).
  if nargin < 2
    k = 1;
  end
  k = mod(k, 4);
  r = a;
  for i = 1:k
    r = flip(transpose(r), 1);
  end
end
%=============================================================================
