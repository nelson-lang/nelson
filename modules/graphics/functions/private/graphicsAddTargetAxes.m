%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ax = graphicsAddTargetAxes(parent)
  % Resolves the axes a scene addition draws into, without applying any
  % NextPlot policy. A function that adds to an existing scene never starts a
  % new plot, so it must not call newplot: whatever is already drawn stays in
  % place, even when NextPlot is 'replace'. Use graphicsTargetAxes() instead
  % for a function that does start a new plot.
  if isempty(parent)
    ax = gca();
  elseif isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
end
%=============================================================================
