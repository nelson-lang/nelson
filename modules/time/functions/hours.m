%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = hours(x)
  narginchk(1, 1);
  if isa(x, 'duration')
    h = double(x) ./ 3600;
  else
    h = duration(double(x), 0, 0);
    h.Format = 'h';
  end
end
%=============================================================================
