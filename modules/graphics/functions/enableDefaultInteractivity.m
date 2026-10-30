%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function enableDefaultInteractivity(ax)
  % enableDefaultInteractivity(ax)
  % turns the built-in axes interactions back on (drag to pan, scroll to zoom)
  narginchk(1, 1);
  if (~isscalar(ax) || ~isgraphics(ax, 'axes'))
    error('Nelson:graphics:interaction:InvalidInputAxes', 'Input must be an axes handle.');
  end
  __default_interactivity__(ax, 'on');
end
%=============================================================================
