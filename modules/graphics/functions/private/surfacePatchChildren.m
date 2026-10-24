%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = surfacePatchChildren(ax)
  h = [];
  if nargin == 0 || ~isgraphics(ax, 'axes')
    return
  end
  children = allchild(ax);
  for k = 1:numel(children)
    child = children(k);
    if isgraphics(child, 'surface') || isgraphics(child, 'functionsurface') || isgraphics(child, 'patch')
      h = [h, child];
    end
  end
end
%=============================================================================
