%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mu = lin2mu(y)
  % https://en.wikipedia.org/wiki/%CE%9C-law_algorithm    
  y = 32768 * y;
  sig = sign(y) + double(y == 0);
  y = min(abs(y), 32635);
  [f, e] = log2(y + 132);
  mu = (64 * sig) - 16 * e - fix(32 * f) + 335;
end
%=============================================================================
