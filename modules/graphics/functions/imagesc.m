%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = imagesc(varargin)
  imageHandle = [];
  leadingParent = (numel(varargin) > 0) && isscalar(varargin{1}) ...
    && (isgraphics(varargin{1}, 'axes') || isgraphics(varargin{1}, 'hggroup'));
  if leadingParent
    parent = varargin{1};
    varargin(1) = [];
    nargin = nargin - 1;
  else
    parent = graphicsParentProperty(varargin);
  end
  if ~isempty(parent)
    if isgraphics(parent, 'hggroup')
      ax = ancestor(parent, 'axes');
    else
      ax = parent;
    end
    if (leadingParent && length(varargin) == 0)
      im = image(ax);
      im.CDataMapping = 'scaled';
      if (nargout > 0)
        varargout{1} = im;
      end
      return
    end
    ax = newplotTarget(ax);
  else
    [ax, imageHandle] = imagescCurrentAxes(nargout);
  end
  if (length(varargin) == 0)
    im = image();
    im.CDataMapping = 'scaled';
    if (nargout > 0)
      varargout{1} = im;
    end
    return
  end
  if (ischar(varargin{1}) || isStringScalar(varargin{1}))
    % Property/value syntax: imagesc('CData', C, 'XData', x, ...)
    ax.YDir = 'reverse';
    if isempty(imageHandle)
      handle = __image__('Parent', ax, varargin{:}, 'CDataMapping', 'scaled');
    else
      if ~isequal(ax.Children, imageHandle)
        ax.Children = imageHandle;
      end
      handle = imageHandle;
      set(handle, varargin{:}, 'CDataMapping', 'scaled');
    end
    axis(ax, 'tight');
    ax.Box = 'on';
    if (nargout > 0)
      varargout{1} = handle;
    end
    return
  end
  lim = [];
  if ((nargin == 2) || (nargin == 4))
    if ((length(varargin{end}) == 2) && varargin{end}(1) <= varargin{end}(2))
      lim = varargin{end};
      varargin(end) = [];
      nargin = nargin - 1;
    end
  end

  if (isempty(imageHandle) && nargout == 0 && strcmp(lower(ax.NextPlot), 'replace') ...
      && ~isempty(ax.Children))
    children = ax.Children;
    for k = 1:numel(children)
      if isgraphics(children(k), 'image')
        imageHandle = children(k);
        break;
      end
    end
  end
  ax.YDir = 'reverse';
  if ((length(varargin) == 1) || ((length(varargin) > 1) && ischar(varargin{2})))
    C = varargin{1};
    if isempty(imageHandle)
      handle = __image__('Parent', ax, 'CData', C, 'XData', [], ...
        'YData', [], varargin{2:end}, 'CDataMapping', 'scaled');
    else
      if ~isequal(ax.Children, imageHandle)
        ax.Children = imageHandle;
      end
      handle = imageHandle;
      set(handle, 'CData', C, 'XData', [], 'YData', [], ...
        varargin{2:end}, 'CDataMapping', 'scaled');
    end
    axis(ax, 'tight');
    ax.Box = 'on';
  elseif (length(varargin) >= 3)
    x = varargin{1};
    y = varargin{2};
    C = varargin{3};
    if isempty(imageHandle)
      handle = __image__('Parent', ax, 'XData', x, 'YData', y, 'CData', C, ...
        varargin{4:end}, 'CDataMapping', 'scaled');
    else
      if ~isequal(ax.Children, imageHandle)
        ax.Children = imageHandle;
      end
      handle = imageHandle;
      set(handle, 'XData', x, 'YData', y, 'CData', C, varargin{4:end}, ...
        'CDataMapping', 'scaled');
    end
    axis(ax, 'tight');
    ax.Box = 'on';
  else
    handle = image(ax, varargin{:}, 'CDataMapping', 'scaled');
  end

  if ~isempty(lim),
    clim(ax, lim);
  end

  if (nargout > 0)
    varargout{1} = handle;
  end
end
%=============================================================================
function [ax, imageHandle] = imagescCurrentAxes(nbOutputs)
  % No target designated: reuse the image already held by the current axes when
  % the NextPlot policy allows it, otherwise start a new plot there.
  imageHandle = [];
  fig = groot().CurrentFigure;
  if isempty(fig)
    ax = newplot();
    return
  end
  ax = gca();
  figNextPlot = lower(fig.NextPlot);
  axNextPlot = lower(ax.NextPlot);
  children = ax.Children;
  if (nbOutputs == 0 && strcmp(axNextPlot, 'replace') && ~isempty(children) ...
      && ~any(strcmp(figNextPlot, {'replace', 'replacechildren', 'replaceall'})))
    for k = 1:numel(children)
      if isgraphics(children(k), 'image')
        imageHandle = children(k);
        break;
      end
    end
  end
  if ((~isempty(children) && isempty(imageHandle)) || ~strcmp(axNextPlot, 'replace') ...
      || any(strcmp(figNextPlot, {'replace', 'replacechildren', 'replaceall'})))
    ax = newplot();
  end
end
%=============================================================================
