%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = minutes(x)
  narginchk(1, 1);
  if isa(x, 'duration')
    m = double(x) ./ 60;
  else
    m = duration(0, double(x), 0);
    m.Format = 'm';
  end
end
%=============================================================================
