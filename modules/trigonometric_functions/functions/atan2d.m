%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = atan2d(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  y = varargin{1};
  x = varargin{2};
  r = 180 ./ pi .* atan2 (y, x);
end
%=============================================================================
