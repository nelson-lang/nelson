%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colormap(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  if (nargin == 0)
    m = colorMapNoRhs();
  else
    if nargin == 1
      m = colorMapOneRhs(varargin{1});
    else
      % nargin == 2
      go = varargin{1};
      map = varargin{2};
      m = colorMapTwoRhs(go, map);
    end
  end
  if nargout == 1
    varargout{1} = m;
  end
end
%=============================================================================
function m = colorMapNoRhs()
  f = gcf();
  m = f.Colormap;
end
%=============================================================================
function m = colorMapOneRhs(arg)
  if isgraphics(arg, 'axes') 
    ax = arg;
    m = ax.Colormap;
    return
  elseif isgraphics(arg, 'figure')
    f =  arg;
    m = f.Colormap;
    return
  else
    f = gcf();
    map = arg;
    if ischar(map) || isStringScalar(map)
      map = convertStringsToChars(map);
      if strcmp(map, 'default')
        map = 'parula';
      end
      m = eval(map);
      f.Colormap = m;
    else
      m = map;
      f.Colormap = map;
    end
    refresh(f);
  end
  %=============================================================================
function m = colorMapTwoRhs(go, map)
  if ischar(map) || isStringScalar(map)
    map = convertStringsToChars(map);
    if strcmp(map, 'default')
      map = 'parula';
    end
    m = eval(map);
    go.Colormap = m;
  else
    m = map;
    go.Colormap = map;
  end
  if ~isgraphics(go, 'Figure')
    go = ancestor(go, 'Figure');
  end
  refresh(go);
end
%=============================================================================
