%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
    if (isempty (map))
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
  isSupported = (isnumeric (im) || islogical (im)) && (nd == 2 || nd == 3);
  if ~isSupported
    error(_('Invalid #1 argument: image or filename expected.'));
  end
  
  if (nd == 2)
    if ~indexed
      usedColormap = gray();
    end
  elseif (size (im, 3) == 3)
    if isdouble(im) || issingle(im) || isuint8(im) || isuint16(im)
      truecolor = true;
    else
      error(_('Invalid #1 argument: image must be uint8, uint16, double, or single.'));
    end
  else
    error(_('Wrong size for #1 argument: image must be MxN or MxNx3 array.'));
  end
  
  displayRange = [];
  XData = [];
  YData = [];
  args = {};
  
  while ~isempty(inputArguments)
    arg = inputArguments{1};
    if isnumeric(arg)
      if (numel(arg) == 2 || isempty (arg))
        displayRange = arg;
      elseif (size(arg, 2) == 3)
        indexed = true;
        if ((min(arg, [], 'all') >= 0) || (max(arg, [], 'all') <= 1))
          usedColormap = arg;
        else
          error (_('Invalid colormap.'));
        end
      elseif ~isempty(arg)
        error(_('Invalid input argument.'));
      end
      inputArguments = inputArguments(3:end);
    elseif (ischar(arg))
      arg = tolower(arg);
      switch(arg)
        case 'border'
          warning(_('border option not yet implement.'));
          inputArguments = inputArguments(3:end);
        case 'initialmagnification'
          warning(_('initialMagnification option not yet implement.'));
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
          if (min (map, [], 'all') >= 0 || max(map, [], 'all') <= 1)
            usedColormap = map;
          else
            error(_('Invalid colormap'));
          end
          inputArguments = inputArguments(3:end);
        case 'reduce'
          warning(_('reduce option not yet implement.'));
          inputArguments = inputArguments(3:end);
        case 'xdata'
          XData = inputArguments{2};
          if ~isvector(XData)
            error(_('XData must be a vector.'));
          end
          inputArguments = inputArguments(3:end);
        case 'ydata'
          YData = inputArguments{2};
          if ~isvector(YData)
            error(_('YData must be a vector.'));
          end
          inputArguments = inputArguments(3:end); 
        otherwise
          error(_('Unrecognized property.'));
        end
      else
        error(_('Invalid argument detected.'));
      end
    end
    if (~isreal(im))
      warning(_('Displaying real part of complex input.'));
      im = real(im);
    end
    if isempty(displayRange)
      displayRange = double([min(im(:)), max(im(:))]);
    end
    if (isfloat (im))
      rnan = isnan(im(:));
      if (any (rnan))
        warning(_('Pixels with NaN values are set to minimum pixel value.'));
        im(rnan) = displayRange(1);
      end
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
    parent.Visible = 'off';
    if isValidGraphicsProperty('figure', 'View')
      parent.View = [0, 90];
    end
    parent.YDir = 'reverse';
    parent.Layer = 'top';
    axis('image');
    if (nargout > 0)
      varargout{1} = h;
    else
      varargout = {};
    end
  end
  %=============================================================================
