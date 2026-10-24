%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lineHandles = graphics_stackedplot_line_handles(ax)
  lineHandles = [];
  % Children run top first, so they are walked the other way up to hand the
  % lines back in the order they were plotted.
  children = flipud(ax.Children(:));
  for k = 1:length(children)
    if isgraphics(children(k), 'line')
      lineHandles = [lineHandles; children(k)];
    end
  end
end
%=============================================================================
