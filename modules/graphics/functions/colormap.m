%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colormap(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  if (nargin == 0)
    f = gcf();
    varargout{1} = f.Colormap;
  else
    if nargin == 1
      if isgraphics(varargin{1}, 'axes') 
        ax = varargin{1};
        varargout{1} = ax.ColorMap;
        return
      elseif isgraphics(varargin{1}, 'figure')
        f =  varargin{1};
        varargout{1} = f.ColorMap;
        return
      else
        f = gcf();
        map = varargin{1};
        if ischar(map) || isStringScalar(map)
          map = convertStringsToChars(map);
          if strcmp(map, 'default')
            map = 'parula';
          end
          f.ColorMap = eval(map);
        else
          f.ColorMap = map;
        end
      end
    else
      % nargin == 2
      go = varargin{1};
      map = varargin{2};
      if ischar(map) || isScalarString(map)
        map = convertStringsToChars(map);
        if strcmp(map, 'default')
          map = 'parula';
        end
        go.ColorMap = eval(map);
      else
        go.Colormap = map;
      end
    end
  end
  refresh(gcf());
  if nargout == 1
    varargout{1} = get(go, 'Colormap');
  end
end
%=============================================================================
