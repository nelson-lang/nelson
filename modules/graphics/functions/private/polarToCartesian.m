%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y] = polarToCartesian(theta, radius)
  reflected = radius < 0;
  theta(reflected) = theta(reflected) + pi;
  radius(reflected) = -radius(reflected);
  x = radius .* cos(theta);
  y = radius .* sin(theta);
end
%=============================================================================
