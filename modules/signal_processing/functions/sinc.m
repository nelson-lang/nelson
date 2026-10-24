%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = sinc(varargin)
  narginchk(1, 1);
  x = varargin{1};
  zeroMask = x == 0;
  scaledX = pi .* x;
  c = sin(scaledX) ./ scaledX;
  c(zeroMask) = 1;
end
%=============================================================================
