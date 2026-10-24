%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ax, args] = polarParseTargetAxes(args)
  if ~isempty(args) && isscalar(args{1}) && (isgraphics(args{1}, 'axes') || isgraphics(args{1}, 'polaraxes'))
    ax = args{1};
    args = args(2:end);
    return
  end
  gr = groot();
  if ~isempty(gr.CurrentFigure)
    ax = gca();
  else
    ax = axes();
  end
end
%=============================================================================
