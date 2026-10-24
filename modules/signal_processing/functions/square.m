%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = square(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  t = varargin{1};
  duty = 50;
  if nargin == 2
    duty = varargin{2};
  end
  phase = mod(t / (2 * pi), 1);
  y = ones(size(t));
  y(phase >= duty / 100) = -1;
end
%=============================================================================
