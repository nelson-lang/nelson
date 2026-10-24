%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = getframe(varargin)
  narginchk(0, 1);
  nargoutchk(0, 2);
  if nargin() == 0
    go = gca();
  else
    go = varargin{1};
  end
  isAxes = isgraphics(go, 'axes') || isgraphics(go, 'polaraxes');
  if ~isAxes && ~isgraphics(go, 'figure')
    error('Nelson:getframe:figureOrAxesExpected', 'figure or axes handle expected.');
  end
  if isAxes
    cdata = getframeAxes(go);
  else
    cdata = getframeFigure(go);
  end
  if nargout() == 2
    varargout{1} = cdata;
    varargout{2} = [];
  else
    varargout{1} = struct('cdata', cdata, 'colormap', []);
  end
end
%=============================================================================
function cdata = getframeAxes(ax)
  gof = ancestor(ax, 'figure');
  cdata = __getframe__(gof);
  units = ax.Units;
  ax.Units = 'pixels';
  pos = ax.Position;
  ax.Units = units;
  idxx1 = min(max(floor(pos(1)), 1), size(cdata, 2));
  idxx2 = min(idxx1 + max(round(pos(3)), 1) - 1, size(cdata, 2));
  idxx = idxx1:idxx2;
  idxy1 = min(max(floor(pos(2)), 1), size(cdata, 1));
  idxy2 = min(idxy1 + max(round(pos(4)), 1) - 1, size(cdata, 1));
  idxy = fliplr(size(cdata, 1) - (idxy1:idxy2) + 1);
  cdata = cdata(idxy, idxx, :);
end
%=============================================================================
function cdata = getframeFigure(f)
  cdata = __getframe__(f);
end
%=============================================================================
