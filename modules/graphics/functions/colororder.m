%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colororder(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseAxis(args);
  if isempty(args)
    varargout{1} = ax.ColorOrder;
    return
  end
  colors = colorOrderValue(args{1});
  ax.ColorOrder = colors;
  ax.ColorOrderIndex = 1;
  updateAutomaticBarColors(ax, colors);
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
end
%=============================================================================
function [ax, args] = parseAxis(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = gca();
  end
end
%=============================================================================
function colors = colorOrderValue(value)
  if ischar(value) || isStringScalar(value)
    colors = namedColorOrder(char(value));
  elseif isnumeric(value) && isreal(value) && size(value, 2) == 3
    colors = double(value);
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
end
%=============================================================================
function colors = namedColorOrder(name)
  switch lower(name)
    case {'default', 'gem'}
      colors = [0.066 0.443 0.745; 0.866 0.329 0; 0.929 0.694 0.125; ...
        0.521 0.086 0.819; 0.231 0.666 0.196; 0.184 0.745 0.937; ...
        0.819 0.015 0.545];
    case 'glow'
      colors = [0.149 0.549 0.866; 0.96 0.466 0.16; 1 0.909 0.392; ...
        0.752 0.36 0.984; 0.286 0.858 0.25; 0.423 0.956 1; ...
        0.949 0.403 0.772];
    case 'sail'
      colors = [0.062 0.258 0.501; 0.329 0.713 1; 1 0.27 0.227; ...
        0.564 0.149 0.133; 0.066 0.443 0.745];
    case 'reef'
      colors = [0.866 0.329 0; 0.329 0.713 1; 0.066 0.443 0.745; ...
        0.996 0.564 0.262; 0.454 0.921 0.854; 0 0.639 0.639];
    case 'meadow'
      colors = [0.007 0.345 0.054; 0.227 0.784 0.192; 1 0.839 0.039; ...
        0.96 0.466 0.16; 0.752 0.298 0.043; 0.98 0.541 0.831; ...
        0.49 0.662 1];
    case 'dye'
      colors = [0.717 0.192 0.172; 0.231 0.666 0.196; 0.368 0.133 0.588; ...
        0.066 0.443 0.745; 0.866 0.329 0; 0.007 0.47 0.501; ...
        0.913 0.317 0.721];
    case 'earth'
      colors = [0.062 0.258 0.501; 0.717 0.192 0.172; 0.611 0.466 0.125; ...
        0.007 0.345 0.054; 0.862 0.6 0.423; 0.372 0.105 0.031; ...
        1 0.819 0.619];
    otherwise
      error('Nelson:colororder:unknownColorOrder', 'Unknown color order.');
  end
end
%=============================================================================
function updateAutomaticBarColors(ax, colors)
  bars = findobj(ax, 'Type', 'bar');
  for k = 1:numel(bars)
    if strcmp(bars(k).FaceColorMode, 'auto')
      index = bars(k).SeriesIndex;
      if isnumeric(index) && isscalar(index)
        row = mod(double(index) - 1, size(colors, 1)) + 1;
        bars(k).FaceColor = colors(row, :);
        bars(k).FaceColorMode = 'auto';
      end
    end
  end
end
%=============================================================================
