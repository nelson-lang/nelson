%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ax = graphicsTargetAxes(parent)
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = newplot(parent);
  end
end
%=============================================================================
