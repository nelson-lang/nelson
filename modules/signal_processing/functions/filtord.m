%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = filtord(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  b = varargin{1};
  a = 1;
  if nargin == 2
    a = varargin{2};
  end
  b = signalTrimTrailingZeros(b(:).');
  a = signalTrimTrailingZeros(a(:).');
  n = max(length(a), length(b)) - 1;
end
%=============================================================================
