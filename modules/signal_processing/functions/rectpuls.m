%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = rectpuls(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  t = varargin{1};
  width = 1;
  if nargin == 2
    width = varargin{2};
  end
  y = double(t >= -width / 2 & t < width / 2);
end
%=============================================================================
