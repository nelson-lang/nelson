%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = getframe(varargin)
  narginchk(0, 1);
  nargoutchk(0, 2);
  if nargin() == 0
    go = gca();
  else
    go = varargin{1};
  end
  isAxes = isgraphics(go, 'axes') || isgraphics(go, 'polaraxes');
  if ~isAxes && ~isgraphics(go, 'figure')
    error('Nelson:getframe:figureOrAxesExpected', 'figure or axes handle expected.');
  end
  if isAxes
    cdata = getframeAxes(go);
  else
    cdata = getframeFigure(go);
  end
  if nargout() == 2
    varargout{1} = cdata;
    varargout{2} = [];
  else
    varargout{1} = struct('cdata', cdata, 'colormap', []);
  end
end
%=============================================================================
function cdata = getframeAxes(ax)
  gof = ancestor(ax, 'figure');
  cdata = __getframe__(gof);
  % position in the figure, also for an axes in a panel, a tab or a tiled layout
  pos = getpixelposition(ax, true);
  frameWidth = size(cdata, 2);
  frameHeight = size(cdata, 1);
  % The frame of a figure is at the display resolution: the edges of the axes, in figure
  % pixels (the first one is 1), are scaled to the frame, then rounded.
  scale = frameScale(gof, frameWidth, frameHeight);
  left = round((pos(1) - 1) * scale(1));
  right = round((pos(1) - 1 + pos(3)) * scale(1));
  bottom = round((pos(2) - 1) * scale(2));
  top = round((pos(2) - 1 + pos(4)) * scale(2));
  left = min(max(left, 0), frameWidth - 1);
  right = min(max(right, left + 1), frameWidth);
  bottom = min(max(bottom, 0), frameHeight - 1);
  top = min(max(top, bottom + 1), frameHeight);
  cdata = cdata(frameHeight - top + 1:frameHeight - bottom, left + 1:right, :);
end
%=============================================================================
% pixels of the frame per pixel of the figure, along x and y
function scale = frameScale(f, frameWidth, frameHeight)
  scale = [1, 1];
  figurePosition = getpixelposition(f);
  if all(figurePosition(3:4) > 0)
    scale = [frameWidth, frameHeight] ./ figurePosition(3:4);
  end
end
%=============================================================================
function cdata = getframeFigure(f)
  cdata = __getframe__(f);
end
%=============================================================================
