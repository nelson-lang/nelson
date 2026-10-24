%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function target = newplotTarget(target)
  % Applies the NextPlot policy of a plotting target designated by the caller.
  % An empty target falls back to the current axes, so this is a drop-in
  % replacement for a bare newplot() call that also honours an explicit target.
  % An hggroup is returned untouched: adding to a group must not clear the
  % axes that holds it.
  if isempty(target)
    target = newplot();
  elseif isgraphics(target, 'axes')
    target = newplot(target);
  end
end
%=============================================================================
