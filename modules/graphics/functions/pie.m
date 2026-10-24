%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pie(varargin)
  nargoutchk(0, 1);
  [ax, X, labels, explode, normalize] = checkPieInputArguments(varargin);

  p = [];
  refinement = 100;
  if (normalize)
    xphi = cumsum(X / sum(X) * 360);
  else
    xphi = cumsum(X * 360);
  end

  lengthX = length(X);
  setAxisCLim(ax, lengthX);
  for i = 1:lengthX
    p = [p, drawSliceOfPie(ax, xphi, i, refinement, explode, labels)];
  end
  setAxisProperties(ax);
  setVisibility(p);
  if nargout == 1
    varargout{1} = p;
  else
    varargout = {};
  end
end
%=============================================================================
function setVisibility(p)
  if ~isempty(p)
    set(p, 'Visible', 'on');
  end
end
%=============================================================================
function setAxisCLim(ax, lengthX)
  if (lengthX == 1)
    ax.CLim = [1, 2];
  else
    ax.CLim = [1, lengthX];
  end
end
%=============================================================================
function align = determineAlignment(xt)
  if xt > 0
    align = 'left';
  elseif xt < 0
    align = 'right';
  else
    align = 'center';
  end
end
%=============================================================================
function [XP, YP] = calculatePatchCoordinates(xoff, yoff, xn)
  XP = xoff + [0, -sind(xn), 0];
  YP = yoff + [0, cosd(xn), 0];
end
%=============================================================================
function setAxisProperties(ax)
  axis(ax, 'off');
  ax.PlotBoxAspectRatio = [1, 1, 1];
  ax.DataAspectRatioMode = 'auto';
  axis(ax, [-1.2, 1.2, -1.2, 1.2]);
  ax.ZLim = [-1, 1];
  ax.ZTick = [-1, 0, 1];
end
%=============================================================================
function validateLabels(haveLabels)
  if haveLabels
    error('Nelson:pie:labelsValuesAlreadyDefined', 'labels values already defined.');
  end
end
%=============================================================================
function validateExplode(haveExplode)
  if haveExplode
    error('Nelson:pie:explodeValuesAlreadyDefined', 'explode values already defined.');
  end
end
%=============================================================================
function labels = createLabels(X, normalize)
  if (normalize)
    XL = round(100 * X ./ sum(X));
  else
    XL = round(100 * X);
  end

  for i = 1:length(X)
    labels{i} = sprintf('%d%%', XL(i));
  end
end
%=============================================================================
function validateSizes(X, labels, explode)
  if (numel(X) ~= numel(labels))
    error('Nelson:pie:sizeMismatchOfX', 'Size mismatch of X and %s on arguments.', 'labels');
  end
  if ~isequal(size(X), size(explode))
    error('Nelson:pie:sizeMismatchOfX', 'Size mismatch of X and %s on arguments.', 'explode');
  end
end
%=============================================================================
function [xn, xn2] = getXn(xphi, i, refinement)
  if (i == 1)
    xn = 0:360 / refinement:xphi(i);
  else
    xn = xphi(i - 1):360 / refinement:xphi(i);
  end

  if (xn(end) ~= xphi(i))
    xn = [xn, xphi(i)];
  end
  xn2 = (xn(1) + xn(end)) / 2;
end
%=============================================================================
function [xoff, yoff] = calculateOffset(explode, xn, xn2, i)
  if (explode(i))
    xoff = -0.1 * sind(xn2);
    yoff = 0.1 * cosd(xn2);
  else
    xoff = 0;
    yoff = 0;
  end
end
%=============================================================================
function [xt, yt] = calculateTextPosition(xn2)
  xt = -1.1 * sind(xn2);
  yt = 1.1 * cosd(xn2);
end
%=============================================================================
function p = drawSliceOfPie(ax, xphi, i, refinement, explode, labels)
  [xn, xn2] = getXn(xphi, i, refinement);
  [xoff, yoff] = calculateOffset(explode, xn, xn2, i);
  [xt, yt] = calculateTextPosition(xn2);
  align = determineAlignment(xt);
  [XP, YP] = calculatePatchCoordinates(xoff, yoff, xn);
  defaultColor = graphicsDefaultEdgeColor();
  p = [patch(ax, XP, YP, i, 'EdgeColor', defaultColor, 'Visible', 'off'), ...
    text(xt, yt, labels{i}, 'Parent', ax, 'Color', defaultColor, ...
    'HorizontalAlignment', align, 'Visible', 'off')];
end
%=============================================================================
function [ax, X, labels, explode, normalize] = checkPieInputArguments(inputArguments)
  nbInputArguments = length(inputArguments);
  if (nbInputArguments >= 2) && isscalar(inputArguments{1}) ...
    && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup'))
    ax = newplotTarget(inputArguments{1}(1));
    inputArguments(1) = [];
    nbInputArguments = nbInputArguments - 1;
  else
    ax = newplotTarget(graphicsParentProperty(inputArguments));
  end
  if nbInputArguments < 1
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nbInputArguments > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  haveExplode = false;
  haveLabels = false;

  X = inputArguments{1};
  X = X(:)';
  if (nbInputArguments > 1)
    arg = inputArguments{2};
    if iscell(arg)
      labels = arg;
      haveLabels = true;
    elseif (isnumeric(arg) || islogical(arg))
      explode = arg;
      haveExplode = true;
    else
      error('Nelson:pie:explodeOrLabelsExpected', 'explode or labels values expected.');
    end
  end
  if (nbInputArguments > 2)
    arg = inputArguments{3};
    if iscell(arg) || isstring(arg)
      validateLabels(haveLabels)
      labels = arg;
      haveLabels = true;
    elseif (isnumeric(arg) || islogical(arg))
      validateExplode(haveExplode)
      explode = arg;
      haveExplode = true;
    else
      error('Nelson:pie:explodeOrLabelsExpected', 'explode or labels values expected.');
    end
  end

  if (~haveExplode)
    explode = zeros(size(X));
  end

  normalize = true;
  if (sum(X(:)) < 1)
    normalize = false;
  end

  if (~haveLabels)
    labels = createLabels(X, normalize);
  end

  validateSizes(X, labels, explode);
end
%=============================================================================
