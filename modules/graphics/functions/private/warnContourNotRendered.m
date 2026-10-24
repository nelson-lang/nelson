%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function warnContourNotRendered(Z)
  % A surface with no relief carries no contour line to draw. The two ways that
  % happens do not read alike and each keeps its own warning: data whose finite
  % values are all equal is constant, data holding no finite value at all is
  % not. Only the plotting functions warn; contourc is silent.
  finiteZ = Z(isfinite(Z));
  if isempty(finiteZ)
    warning('Nelson:contour:nonFiniteData', 'Contour not rendered for non-finite ZData.');
  elseif min(finiteZ(:)) == max(finiteZ(:))
    warning('Nelson:contour:constantData', 'Contour not rendered for constant ZData.');
  end
end
%=============================================================================
