%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = sawtooth(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  t = varargin{1};
  width = 1;
  if nargin == 2
    width = varargin{2};
  end
  phase = mod(t / (2 * pi), 1);
  y = zeros(size(t));
  rising = phase < width;
  y(rising) = 2 * phase(rising) / width - 1;
  y(~rising) = 1 - 2 * (phase(~rising) - width) / (1 - width);
end
%=============================================================================
