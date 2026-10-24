%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function shg()
  narginchk(0, 0);
  % Show the current graph window: bring the current figure forward, creating
  % one if none exists.
  figure(gcf());
end
%=============================================================================
