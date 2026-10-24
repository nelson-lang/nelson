%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = view(varargin)
  % view(2), view(3)
  % view([0 90]), view([0 90 0])
  % view(az, el)

  narginchk(0, 3);
  nargoutchk(0, 2);
  args = {};

  if (nargin == 0)
    ax = gca();
    args = {ax};
  elseif (nargin == 1)
    if (isgraphics(varargin{1}, 'axes'))
      % view(ax) queries that axes, without touching the current figure
      args = {varargin{1}};
    else
      ax = gca();
      args = {ax, varargin{1}};
    end
  elseif (nargin == 2)
    if (isgraphics(varargin{1}))
      args = {varargin{1}, varargin{2}};
    else
      ax = gca();
      args = {ax, varargin{1}, varargin{2}};
    end
  else
    args = varargin;
  end
  if (nargout == 0 && numel(args) == 3 && isgraphics(args{1}, 'axes'))
    currentView = args{1}.View;
    if (isequal(currentView, [args{2}, args{3}]))
      varargout = {};
      return
    end
  end
  [azimuth, elevation] = __view__(args{:});
  if (nargout == 0)
    varargout = {};
  elseif (nargout == 1)
    varargout{1} = [azimuth, elevation];
  else
    varargout{1} = azimuth;
    varargout{2} = elevation;
  end
end
%=============================================================================
