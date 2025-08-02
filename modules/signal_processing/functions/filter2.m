%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function f = filter2(varargin)
  narginchk(2, 3);
  b = varargin{1};
  x = varargin{2};
  if (nargin < 3)
    shape = 'same';
  else
    shape = varargin{3};
  end
  [r, c] = size(b);
  f = conv2(x, b(r:-1:1, c:-1:1), shape);
end
%=============================================================================
