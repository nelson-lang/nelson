%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = newplot(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);

  gr = groot();
  fig = gr.CurrentFigure;
  applyFigureNextPlot = false;
  if (nargin() == 1)
    ax = varargin{1};
    if isempty(ax)
      ax = gca();
      % gca() may have just created the figure; refresh the stale (empty)
      % CurrentFigure captured above so fig.NextPlot below is valid.
      fig = gr.CurrentFigure;
    elseif isgraphics(ax, 'figure')
      fig = ax;
      ax = fig.CurrentAxes;
      applyFigureNextPlot = true;
      if isempty(ax)
        ax = axes(fig);
      end
    else
      fig = ancestor(ax, 'figure');
    end
  else
    ax = gca();
    fig = gr.CurrentFigure;
  end

  figNextPlot = lower(fig.NextPlot);
  if (applyFigureNextPlot && any(strcmp(figNextPlot, {'replace', 'replacechildren', 'replaceall'})))
    clf(fig);
    ax = gca();
    fig = gr.CurrentFigure;
  end

  axNextPlot = lower(ax.NextPlot);
  if (strcmp(axNextPlot, 'replacechildren'))
    cla(ax);
  elseif (strcmp(axNextPlot, 'replace'))
    % 'replace' clears the children and restores the auto limit/tick modes to
    % their defaults (Position and Units are preserved). Without this a manual
    % mode left over from a previous 'axis manual' would freeze the limits and
    % the next plot would overflow its box.
    cla(ax);
    resetAxesAutoModes(ax);
  elseif (strcmp(axNextPlot, 'replaceall'))
    reset(ax);
    cla(ax);
  end
  if (nargin() == 1)
    h = ax;
  else
    h = gca();
  end
  if (nargout == 1)
    varargout{1} = h;
  end
end
%=============================================================================
function resetAxesAutoModes(ax)
  modeProperties = {'XLimMode', 'YLimMode', 'ZLimMode', 'CLimMode', 'ALimMode', ...
    'XTickMode', 'YTickMode', 'ZTickMode', 'XTickLabelMode', 'YTickLabelMode', ...
    'ZTickLabelMode', 'DataAspectRatioMode', 'PlotBoxAspectRatioMode', ...
    'CameraPositionMode', 'CameraTargetMode', 'CameraUpVectorMode', ...
    'CameraViewAngleMode'};
  for k = 1:numel(modeProperties)
    name = modeProperties{k};
    if isprop(ax, name)
      set(ax, name, 'auto');
    end
  end
end
%=============================================================================
