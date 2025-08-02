%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = axis(varargin);
  if (length(varargin) > 0)
    if (isgraphics(varargin{1}, 'axes'))
      ax = varargin{1};
      varargin(1) = [];
    else
      ax = gca();
    end
  else
    ax = gca();
  end
  saveca = gca();
  axes(ax);
  if (length(varargin) == 0)
    if (is2D(ax))
      varargout{1} = [ax.XLim, ax.YLim];
    else 
      varargout{1} = [ax.XLim, ax.YLim, ax.ZLim];
    end
  else
    mode = varargin{1};
    if (isnumeric(mode))
      axisNumericMode(ax, mode);
    elseif (strcmp(mode, 'maximal'))
      axisMaximalMode(ax);
    elseif (strcmp(mode, 'manual'))
      axisManualMode(ax);
    elseif (strcmp(mode, 'tight'))
      axisTightMode(ax);
    elseif (strcmp(mode, 'fill'))
      axisFillMode(ax);
    elseif (strcmp(mode, 'ij'))
      axisIJMode(ax);
    elseif (strcmp(mode, 'xy'))
      axisXYMode(ax);
    elseif (strcmp(mode, 'equal'))
      axisEqualMode(ax);
    elseif (strcmp(mode, 'image'))
      axisImageMode(ax);
    elseif (strcmp(mode, 'square'))
      axisSquareMode(ax);
    elseif (strcmp(mode, 'normal'))
      axisNormalMode(ax);
    elseif (strcmp(mode, 'vis3d'))
      axis3DMode(ax);
    elseif (strcmp(mode, 'off'))
      axisOffMode(ax);
    elseif (strcmp(mode, 'on'))
      axisOnMode(ax);
    elseif (strncmp(mode, 'auto', 4))
      axisAutoMode(ax,mode);
    else
      error([_('Unknown command option:'), mode]);
    end
  end
  axes(saveca);
end
%=============================================================================
function axisOnMode(ax)
  ax.Visible = 'on';
end
%=============================================================================
function axisOffMode(ax)
  ax.Visible = 'off';
end
%=============================================================================
function axis3DMode(ax)
  ax.PlotBoxAspectRatioMode = 'manual';
  ax.DataAspectRatioMode = 'manual';
end
%=============================================================================
function axisNormalMode(ax)
  ax.PlotBoxAspectRatioMode = 'auto';
  ax.DataAspectRatioMode = 'auto';
end
%=============================================================================
function axisSquareMode(ax)
  ax.PlotBoxAspectRatio = [1, 1, 1];
  ax.DataAspectRatioMode = 'auto';
end
%=============================================================================
function axisImageMode(ax)
  ax.DataAspectRatio = [1, 1, 1];
  ax.PlotBoxAspectRatioMode = 'auto';
  lims = ax.DataLimits;
  ax.XLim = lims(1:2);
  ax.YLim = lims(3:4);
  ax.ZLim = lims(5:6);
end
%=============================================================================
function axisEqualMode(ax)
  ax.DataAspectRatio = [1,1,1];
end
%=============================================================================
function axisXYMode(ax)
  ax.XDir = 'normal';
  ax.YDir = 'normal';
end
%=============================================================================
function axisIJMode(ax)
  ax.XDir = 'normal';
  ax.YDir = 'reverse';
end
%=============================================================================
function axisTightMode(ax)
  lims = ax.DataLimits;
  ax.XLim = lims(1:2);
  ax.YLim = lims(3:4);
  ax.ZLim = lims(5:6);
end
%=============================================================================
function axisManualMode(ax)
  ax.XLimMode = 'manual';
  ax.YLimMode = 'manual';
  ax.ZLimMode = 'manual';
end
%=============================================================================
function axisMaximalMode(ax)
  ax.XTick = [];
  ax.YTick = [];
  ax.ZTick = [];
  ax.XTickLabel = '';
  ax.YTickLabel = '';
  ax.ZTickLabel = '';
end  
%=============================================================================
function axisAutoMode(ax, mode)
  allActive = strcmp(mode, 'auto');
  xActive = any(find(mode == 'x'));
  yActive = any(find(mode == 'y'));
  zActive = any(find(mode == 'z'));
  if (allActive | xActive), ax.XLimMode = 'auto'; end
  if (allActive | yActive), ax.YLimMode = 'auto'; end
  if (allActive | zActive), ax.ZLimMode ='auto'; end
end
%=============================================================================
function axisNumericMode(ax, mode)
  mode = mode(:);
  if (length(mode) == 2)
    ax.XLim = mode;
  elseif (length(mode) == 4)
    ax.XLim = mode(1:2);
    ax.YLim = mode(3:4);
  elseif (length(mode) == 6)
    ax.XLim = mode(1:2);
    ax.YLim = mode(3:4);
    ax.ZLim = mode(5:6);
  elseif (length(mode) == 8)
    ax.XLim = mode(1:2);
    ax.YLim = mode(3:4);
    ax.ZLim = mode(5:6);
    ax.CLim = mode(7:8);
  else
    error(_('Vector must have 4, 6, or 8 elements.'));
  end
end
%=============================================================================
function axisFillMode(ax)
  pos = ax.Position;
  parent = ax.Parent;
  parentPosition = parent.Position;
  width = pos(3) * parentPosition(3);
  height = pos(3) * parentPosition(4);
  ax.PlotBoxAspectRatio = [width, height, 1];
end
%=============================================================================
