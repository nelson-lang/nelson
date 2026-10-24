%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = imshow(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  indexed = false;
  truecolor = false;
  usedColormap = [];
  inputArguments = varargin;

  if (ischar(inputArguments{1}) || isStringScalar(inputArguments{1}))
    filename = convertStringsToChars(inputArguments{1});

    [im, map] = imread(filename);
    if (isempty(map))
      indexed = false;
    else
      indexed = true;
      usedColormap = map;
    end
  else
    im = inputArguments{1};
  end
  inputArguments = inputArguments(2:end);
  nd = ndims(im);
  isSupported = (isnumeric(im) || islogical(im)) && (nd == 2 || nd == 3);
  if ~isSupported
    error('Nelson:imshow:invalidImageArgument', 'Invalid #%d argument: image or filename expected.', 1);
  end

  if (nd == 2)
    if ~indexed
      usedColormap = gray();
    end
  elseif (size(im, 3) == 3)
    if isdouble(im) || issingle(im) || isuint8(im) || isuint16(im)
      truecolor = true;
    else
      error('Nelson:imshow:invalidImage', 'Invalid #%d argument: image must be uint8, uint16, double, or single.', 1);
    end
  else
    error('Nelson:imshow:invalidImageSize', 'Wrong size for #%d argument: image must be MxN or MxNx3 array.', 1);
  end

  displayRange = [];
  XData = [];
  YData = [];
  initialMag = [];
  borderTight = false;
  args = {};

  while ~isempty(inputArguments)
    arg = inputArguments{1};
    if isnumeric(arg)
      if (numel(arg) == 2 || isempty(arg))
        displayRange = arg;
      elseif (size(arg, 2) == 3)
        indexed = true;
        if ((min(arg, [], 'all') >= 0) && (max(arg, [], 'all') <= 1))
          usedColormap = arg;
        else
          error('Nelson:imshow:invalidColormap', 'Invalid colormap.');
        end
      elseif ~isempty(arg)
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
      end
      inputArguments = inputArguments(2:end);
    elseif (ischar(arg))
      arg = tolower(arg);
      switch (arg)
        case 'border'
          borderValue = inputArguments{2};
          if (ischar(borderValue) || isStringScalar(borderValue))
            borderTight = strcmp(tolower(convertStringsToChars(borderValue)), 'tight');
          end
          inputArguments = inputArguments(3:end);
        case 'initialmagnification'
          initialMag = inputArguments{2};
          inputArguments = inputArguments(3:end);
        case 'displayrange'
          displayRange = inputArguments{2};
          inputArguments = inputArguments(3:end);
        case 'parent'
          args{end + 1} = 'parent';
          args{end + 1} = inputArguments{2};
          inputArguments = inputArguments(3:end);
        case 'colormap'
          map = inputArguments{2};
          if (min(map, [], 'all') >= 0 && max(map, [], 'all') <= 1)
            usedColormap = map;
          else
            error('Nelson:imshow:invalidColormap', 'Invalid colormap.');
          end
          inputArguments = inputArguments(3:end);
        case 'reduce'
          warning('Nelson:imshow:reduceNotImplemented', 'reduce option not yet implement.');
          inputArguments = inputArguments(3:end);
        case 'xdata'
          XData = inputArguments{2};
          if ~isvector(XData)
            error('Nelson:validators:mustBeVector', '%s must be a vector.', 'XData');
          end
          inputArguments = inputArguments(3:end);
        case 'ydata'
          YData = inputArguments{2};
          if ~isvector(YData)
            error('Nelson:validators:mustBeVector', '%s must be a vector.', 'YData');
          end
          inputArguments = inputArguments(3:end);
        otherwise
          error('Nelson:imshow:unrecognizedProperty', 'Unrecognized property.');
      end
    else
      error('Nelson:imshow:invalidArgumentDetected', 'Invalid argument detected.');
    end
  end

  if (~isreal(im))
    warning('Nelson:imshow:complexInputRealPart', 'Displaying real part of complex input.');
    im = real(im);
  end
  if isempty(displayRange)
    if (islogical(im) || isfloat(im))
      displayRange = [0 1];
    elseif (isinteger(im))
      displayRange = double([intmin(class(im)), intmax(class(im))]);
    else
      displayRange = double([min(im(:)), max(im(:))]);
    end
  end
  if (isfloat(im))
    rnan = isnan(im(:));
    if (any(rnan))
      warning('Nelson:imshow:nanPixelsSetToMin', 'Pixels with NaN values are set to minimum pixel value.');
      im(rnan) = displayRange(1);
    end
  end

  % imshow replaces what the target axes holds. image() and imagesc() are
  % called below in their low-level property/value form, which deliberately
  % leaves the axes alone, so the reset happens here instead.
  parentIndex = find(strcmpi(args, 'Parent'), 1, 'last');
  if isempty(parentIndex)
    newplot();
  else
    imshowResetAxes(args{parentIndex + 1});
  end

  if (truecolor || indexed)
    h = image('CData', im, 'XData', XData, 'YData', YData, args{:});
  else
    h = imagesc('CData', im, 'XData', XData, 'YData', YData, args{:});
    clim(displayRange);
    parent = h.Parent;
    parent.CLim = displayRange;
  end
  parent = h.Parent;
  if ~isempty(usedColormap)
    parent.Colormap = usedColormap;
  end
  if isValidGraphicsProperty('figure', 'View')
    parent.View = [0, 90];
  end
  parent.YDir = 'reverse';
  parent.Layer = 'top';

  % When imshow owns a fresh, default-positioned axes (no explicit 'Parent',
  % no subplot, InitialMagnification not 'fit'), the expected behavior is to
  % size the axes to the image at the requested magnification (default 100%,
  % reduced only when the image is too large for the screen) and resize the
  % figure around it. This keeps a small image small instead of stretching it
  % to fill the whole figure.
  magIsFit = (ischar(initialMag) || isStringScalar(initialMag)) && ...
    strcmp(tolower(convertStringsToChars(initialMag)), 'fit');
  if (~isempty(initialMag) && ~magIsFit && isnumeric(initialMag) && isscalar(initialMag))
    magFraction = double(initialMag) / 100;
  else
    magFraction = 1;
  end
  explicitParent = any(strcmpi(args, 'parent'));
  if (~explicitParent && ~magIsFit && isSoleDefaultAxes(parent))
    fitFigureToImage(parent, size(im, 1), size(im, 2), magFraction, borderTight);
  end

  axis(parent, 'image');
  parent.Visible = 'off';
  if (nargout > 0)
    varargout{1} = h;
  else
    varargout = {};
  end
end
%=============================================================================
function imshowResetAxes(parent)
  if isgraphics(parent, 'hggroup')
    parent = ancestor(parent, 'axes');
  end
  if ~isempty(parent) && (isgraphics(parent, 'axes') || isgraphics(parent, 'figure'))
    newplot(parent);
  end
end
%=============================================================================
function tf = isSoleDefaultAxes(ax)
  % True when 'ax' is the only axes of its figure and still sits at the default
  % axes position, i.e. imshow created it and no subplot or manual layout is in
  % effect. In that case imshow is free to resize the figure around the image.
  tf = false;
  fig = ancestor(ax, 'figure');
  if isempty(fig)
    return
  end
  children = fig.Children;
  axesCount = 0;
  for k = 1:numel(children)
    if isgraphics(children(k), 'axes')
      axesCount = axesCount + 1;
    end
  end
  if (axesCount ~= 1)
    return
  end
  oldUnits = ax.Units;
  restoreUnits = ~strcmp(oldUnits, 'normalized');
  if restoreUnits
    ax.Units = 'normalized';
  end
  pos = ax.Position;
  if restoreUnits
    ax.Units = oldUnits;
  end
  defaultPosition = [0.13, 0.11, 0.775, 0.815];
  tf = all(abs(pos(:)' - defaultPosition) < 5e-3);
end
%=============================================================================
function fitFigureToImage(ax, rows, cols, magFraction, borderTight)
  % Size the axes to the image at 'magFraction' magnification (reduced when the
  % image cannot fit on screen) and resize the figure around it, leaving the
  % default 'loose' border unless a 'tight' border was requested.
  fig = ancestor(ax, 'figure');
  if isempty(fig) || (rows <= 0) || (cols <= 0)
    return
  end
  % Border metrics of the default 'loose' layout (left/bottom offsets and the
  % total margins added to the image to obtain the figure size), plus a minimum
  % figure size. A 'tight' border wraps the figure exactly around the image.
  if borderTight
    leftMargin = 0;
    bottomMargin = 0;
    horizontalMargin = 0;
    verticalMargin = 0;
    minWidth = 1;
    minHeight = 1;
  else
    leftMargin = 84.4;
    bottomMargin = 58.33;
    horizontalMargin = 168;
    verticalMargin = 81;
    minWidth = 128;
    minHeight = 128;
  end
  displayWidth = cols * magFraction;
  displayHeight = rows * magFraction;
  % Reduce the magnification when the image cannot fit on screen at the
  % requested size (the default 100% otherwise upscales huge images off-screen).
  screenSize = groot().ScreenSize;
  if (numel(screenSize) == 4 && screenSize(3) > 1 && screenSize(4) > 1)
    availableWidth = screenSize(3) * 0.69 - horizontalMargin;
    availableHeight = screenSize(4) * 0.69 - verticalMargin;
    if ((displayWidth > availableWidth) || (displayHeight > availableHeight)) ...
        && (availableWidth > 0) && (availableHeight > 0)
      fitScale = min(availableWidth / cols, availableHeight / rows);
      if (fitScale < magFraction)
        displayWidth = cols * fitScale;
        displayHeight = rows * fitScale;
      end
    end
  end
  figureWidth = max(displayWidth + horizontalMargin, minWidth);
  figureHeight = max(displayHeight + verticalMargin, minHeight);
  figurePosition = fig.Position;
  set(fig, 'Position', [figurePosition(1), figurePosition(2), figureWidth, figureHeight]);
  set(ax, 'Units', 'pixels');
  set(ax, 'Position', [leftMargin, bottomMargin, displayWidth, displayHeight]);
  set(ax, 'Units', 'normalized');
end
%=============================================================================
