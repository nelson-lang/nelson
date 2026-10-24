%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = hamming(varargin)
  % https://www.sciencedirect.com/topics/engineering/hamming-window
  narginchk(1, 2);
  [m, N] = signalParseWindowLengthAndOption(varargin);

  if isempty(m)
    c = zeros(0, 1);
  elseif (m ~= 1)
    m = m - 1;
    c = 0.54 - (0.46 * cos(2 * pi * (0:m)' / N));
  else
    c = 1;
  end
end
%=============================================================================
