%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = mu2lin(mu)
  % https://en.wikipedia.org/wiki/%CE%9C-law_algorithm
  mu = 255 - mu;
  sig = double(mu > 127);
  f = rem(mu, 16);
  e = fix(mu / 16) - 8 * sig + 1;    
  y = pow2(f, e + 2);
  e_table = [0 132 396 924 1980 4092 8316 16764];    
  e(:) = e_table(e);
  scale_factor = 1 / 32768;    
  y = scale_factor * (1 - 2 * sig) .* (e + y);
end
%=============================================================================