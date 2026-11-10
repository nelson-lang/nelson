%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = tand(x)
  % exact sind and cosd give exact zeros, signed infinities and +/-1 at 45 degrees
  r = sind(x) ./ cosd(x);
end
%=============================================================================
