%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Write the full web scene of a figure to a file as a self-contained JSON
% document. A replayer with no live engine (a notebook output) can render it.
% Internal figure/notebook plumbing, not part of the public graphics surface.
function saveFigureScene(fig, filename)
  narginchk(2, 2);
  nargoutchk(0, 0);
  __web_figure_scene__(fig, filename);
end
%=============================================================================
