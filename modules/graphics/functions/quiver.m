%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = quiver(varargin)
  narginchk(2, 10000);
  nargoutchk(0, 1);
  inputArguments = varargin;
  if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    ax = inputArguments{1};
    if isgraphics(ax, 'hggroup')
      ax = ancestor(ax, 'axes');
    end
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  scale = 1;
  if (nargin == 2 || nargin == 3)
    u = inputArguments{1};
    v = inputArguments{2};
    [x, y] = meshgrid(1:size(u, 2), 1:size(u, 1));
    if nargin == 3 && isnumeric(inputArguments{3})
      if inputArguments{3} == 0
        scale = 0;
      else
        scale = inputArguments{3};
      end
      inputArguments(3) = [];
    end   
    inputArguments(1:2) = [];
  elseif (nargin >= 4)
    x = inputArguments{1};
    y = inputArguments{2};
    u = inputArguments{3};
    v = inputArguments{4};
    if length(inputArguments) >=5 && isnumeric(inputArguments{5})
      if inputArguments{5} == 0
        scale = 0;
      else
        scale = inputArguments{5};
      end
      inputArguments(5) = [];
    end   
    inputArguments(1:4) = [];
  end
  length = sqrt(u(:) .^ 2+ v(:) .^ 2);
  if scale ~= 0
    length = 0.7 * scale * length * sqrt(max(x(2:end)-x(1:end-1))^2 + max(y(2:end)-y(1:end-1))^2) / max(length(:));
  end
  phi = atan2(v(:), u(:));
  if isempty(inputArguments)
    inputArguments = {'b-'};
  end
  t1 = length .* cos(phi);
  t2 = length .* sin(phi);
  X = [x(:), x(:)+t1]';
  Y = [y(:), y(:)+t2]';
  h = hggroup(ax);
  h.Visible = 'off';
  go1 = plot(h, X, Y, inputArguments{:}, 'Visible', 'off');
  X = [x(:) + t1 - 0.2 * length .* cos(phi - pi / 8), x(:) + t1, x(:) + t1 - 0.2 * length .* cos(phi + pi / 8)]';
  Y = [y(:) + t2 - 0.2 * length .* sin(phi - pi / 8), y(:) + t2, y(:) + t2 - 0.2 * length .* sin(phi + pi / 8)]';
  go2 = plot(h, X, Y, inputArguments{:}, 'Visible', 'off');
  h.Visible = 'on';
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
