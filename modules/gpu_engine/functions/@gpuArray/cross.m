%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = cross(a, b, varargin)
  % cross product of two 3-element vectors, composed from the device overloads.
  if nargin > 2 || numel(a) ~= 3 || numel(b) ~= 3
    % other shapes and the dimension argument are computed on the host
    c = gpuArray(cross(gather(a), gather(b), varargin{:}));
    return
  end
  a1 = a(1); a2 = a(2); a3 = a(3);
  b1 = b(1); b2 = b(2); b3 = b(3);
  c1 = a2 .* b3 - a3 .* b2;
  c2 = a3 .* b1 - a1 .* b3;
  c3 = a1 .* b2 - a2 .* b1;
  sz = size(a);
  if sz(1) == 1
    c = [c1, c2, c3];
  else
    c = [c1; c2; c3];
  end
end
%=============================================================================
