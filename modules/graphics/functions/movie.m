%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = movie(varargin)
  % movie(M)
  % movie(M,n)
  % movie(M,n,fps)
  % movie(h,...)

  narginchk(1, 4);
  nargoutchk(0, 0);
  inputArguments = varargin;
  nbInputArguments = nargin;
  if (isgraphics(varargin{1}, 'axes'))
    ipos = 2;
    ax = varargin{1};
    inputArguments = varargin(2:end);
    nbInputArguments = length(inputArguments);
    if nbInputArguments == 0
      error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  else
    ipos = 1;
    ax = gca();
  end
  M = inputArguments{1};
  mustBeA(M, 'struct');
  if (~isequal(fieldnames(M), {'cdata'; 'colormap'}))
    error('Nelson:movie:invalidStructureFields', 'Invalid structure fields.');
  end
  n = 1;
  fps = 12;
  idx = 1:numel(M);

  if nbInputArguments >= 2
    n = inputArguments{2};
    mustBeInteger(n, ipos + 1);
    mustBeScalarOrEmpty(n, ipos + 1);
    if nbInputArguments >= 3
      fps = inputArguments{3};
      mustBeInteger(fps, ipos + 2);
      mustBeScalarOrEmpty(fps, ipos + 2);
      mustBePositive(fps, ipos + 2);
    end
  end

  if (n > 0)
    idx = repmat(idx, 1, n);
  else
    n = -n;
    mat = repmat([idx, fliplr(idx)], 1, fix(n / 2));
    if (fix(n / 2) ~= n / 2)
      idx = [mat, idx];
    else
      idx = mat;
    end
  end

  hold_state = ishold(ax);
  hold(ax, 'on');

  xLim = ax.XLim;
  yLim = ax.YLim;

  im = [];
  for child = ax.Children
    if (isequal(child.Type, 'image') && (strcmp(child.Tag, 'MovieFrame') == true))
      im = child;
      im.CData = M(1).cdata;
      break;
    end
  end
  if isempty(im)
    im = image('Parent', ax, 'CData', M(1).cdata, 'Tag', 'MovieFrame');
  end
  axis(ax, 'off');
  % Row 1 of CData is the top of the picture, so YData runs from the upper limit
  % down to the lower one. Pin the limits too: an image stretched to the limits
  % pushes them outwards frame after frame if they are left on automatic.
  im.XData = xLim;
  im.YData = [yLim(2), yLim(1)];
  ax.XLim = xLim;
  ax.YLim = yLim;

  tau = 1 / fps;
  timerStart = tic;

  for k = idx
    im.CData = M(k).cdata;

    if ~isempty(M(k).colormap)
      ax.Colormap = M(k).colormap;
    end

    drawnow();
    elapsedTime = toc(timerStart);
    pause(max(0, tau - elapsedTime));
    timerStart = tic;
  end

  if ~hold_state
    hold(ax, 'off');
  end
end

