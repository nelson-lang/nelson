%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [fig, previousDrawLater] = polarBeginDrawLater(ax)
  fig = [];
  previousDrawLater = '';
  try
    fig = ancestor(ax, 'figure');
  catch
    fig = [];
  end
  if ~isempty(fig) && isgraphics(fig, 'figure')
    previousDrawLater = fig.DrawLater;
    if strcmp(previousDrawLater, 'off')
      fig.DrawLater = 'on';
    end
  end
end
%=============================================================================
