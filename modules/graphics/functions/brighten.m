%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = brighten(arg1, arg2)
  % Brighten or darken a colormap.
  %   brighten(beta)           brighten (beta>0) or darken (beta<0) the current colormap
  %   brighten(ax, beta)       operate on the colormap of axes/figure ax
  %   newmap = brighten(beta)  return the brightened current colormap (do not apply)
  %   newmap = brighten(map, beta)  brighten the given colormap
  narginchk(1, 2);
  applyTo = [];
  if nargin == 2
    if isscalar(arg1) && isgraphics(arg1)
      applyTo = arg1;
      map = colormap(arg1);
      beta = arg2;
    else
      map = arg1;
      beta = arg2;
    end
  else
    beta = arg1;
    applyTo = gcf();
    map = colormap(applyTo);
  end
  if ~isscalar(beta) || ~isreal(beta) || abs(beta) >= 1
    error('Nelson:brighten:invalidBeta', 'BETA must be a scalar in the interval (-1, 1).');
  end
  % At beta -> 1 the map goes to white, at beta -> -1 to black, and beta = 0
  % leaves it unchanged; a power law with this gamma matches those endpoints.
  if beta >= 0
    gamma = 1 - beta;
  else
    gamma = 1 / (1 + beta);
  end
  newmap = min(1, max(0, map .^ gamma));
  if nargout == 0
    if isempty(applyTo)
      applyTo = gcf();
    end
    colormap(applyTo, newmap);
  else
    varargout{1} = newmap;
  end
end
%=============================================================================
