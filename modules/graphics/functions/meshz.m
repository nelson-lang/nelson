%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = meshz(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);
  inputArguments = varargin;
  nbInputArguments = nargin;
  if (nbInputArguments >= 2)
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
      go = inputArguments{1}(1);
      inputArguments = inputArguments(2:end);
      nbInputArguments = length(inputArguments);
    else   
      go = newplot();
    end
  else
    go = newplot();
  end
  saveca = gca();
  if isgraphics(go, 'hggroup')
    ax = ancestor(go, 'axes');
    axes(ax);
  else
    axes(go);
  end
  propertiesList = {};
  firstString = find (cellfun ('isclass', inputArguments, 'char'), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  propertiesList = inputArguments(firstString:end);
  inputArguments = inputArguments(1:firstString-1);
  if (length(inputArguments) == 1)
    Z = inputArguments{1};
    [m, n] = size(Z);
    [X, Y] = meshgrid(0:n-1,0:m-1);
    C = Z;
  elseif (length(inputArguments) == 2)
    X = inputArguments{1};
    Y = inputArguments{2};
    Z = X;
    C = Y;
    [m, n] = size(Z);
    [X, Y] = meshgrid(0:n-1, 0:m-1);
  elseif (length(inputArguments) == 3)
    X = inputArguments{1};
    Y = inputArguments{2};
    Z = inputArguments{3};
    C = Z;
  elseif (length(inputArguments) == 4)
    X = inputArguments{1};
    Y = inputArguments{2};
    Z = inputArguments{3};
    C = inputArguments{4};
  else
    error(_('Invalid parameter/value pair arguments.'));
  end
  [X, Y, Z, C] = reworkData(X, Y, Z, C);
  h = mesh(X, Y, Z, C, propertiesList{:});
  if (nargout > 0)
    varargout{1} = h;
  end
end
%=============================================================================
function [X, Y, Z, C] = reworkData(X, Y, Z, C)
  if (isvector (X) && isvector (Y))
    X = [X(1), X(1), X(:) .', X(end), X(end)];
    Y = [Y(1); Y(1); Y(:); Y(end); Y(end)];
  else
    X = [X(1, 1), X(1, 1), X(1, :), X(1, end), X(1, end);
    X(1, 1), X(1, 1), X(1, :), X(1, end), X(1, end);
    X(:, 1), X(:, 1), X, X(:, end), X(:, end);
    X(end, 1), X(end, 1), X(end, :), X(end, end), X(end, end);
    X(end, 1), X(end, 1), X(end, :), X(end, end), X(end, end)];
    
    Y = [Y(1, 1), Y(1, 1), Y(1, :), Y(1, end), Y(1, end);
    Y(1, 1), Y(1, 1), Y(1, :), Y(1, end), Y(1, end);
    Y(:, 1), Y(:, 1), Y, Y(:, end), Y(:, end);
    Y(end, 1), Y(end, 1), Y(end, :), Y(end, end), Y(end, end);
    Y(end, 1), Y(end, 1), Y(end, :), Y(end, end), Y(end, end)];
  end
  ZREF = min (Z(isfinite(Z)));
  Z = [ZREF .* ones(1, size(Z, 2) + 4);
  ZREF .* ones(1, 2), Z(1,:), ZREF .* ones(1, 2);
  ZREF .* ones(size(Z, 1), 1), Z(:,1),Z, Z(:,end), ZREF .* ones(size(Z, 1), 1);
  ZREF .* ones(1, 2), Z(end,:), ZREF .* ones(1, 2);
  ZREF .* ones(1, size(Z, 2) + 4)];
  
  CDATA = C(isfinite (C(:)));
  CREF = (min (CDATA) + max (CDATA)) / 2;
  C = [CREF .* ones(2, size(C, 2) + 4);
  CREF .* ones(size(C, 1), 2), C, CREF .* ones(size(C, 1), 2);
  CREF .* ones(2, size(C, 2) + 4)];
end
%=============================================================================
