%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surface(varargin)
  % surface(X, Y, Z)
  % surface(X, Y, Z, C)
  % surface(Z)
  % surface(Z, C)
  % surface(ax, ...) 
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
  if (length(inputArguments) == 0)
    h = surfaceNoRhs(go, propertiesList);
  elseif (length(inputArguments) == 1)
    h = surfaceOneRhs(go, inputArguments, propertiesList);
  elseif (length(inputArguments) == 2)
    h = surfaceTwoRhs(go, inputArguments, propertiesList);
  elseif (length(inputArguments) == 3)
    h = surfaceThreeRhs(go, inputArguments, propertiesList);
  elseif (length(inputArguments) == 4)
    h = surfaceFourRhs(go, inputArguments, propertiesList);
  else
    error(_('Invalid parameter/value pair arguments.'));
  end
  axes(saveca);
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function h = surfaceNoRhs(go, propertiesList)
  Z = eye(3, 2);
  h = __surf__('Parent', go, 'ZData', Z, propertiesList);
  xlim(go, [1 3]);
  ylim(go, [1 3]);
end
%=============================================================================
function h = surfaceOneRhs(go, inputArguments, propertiesList)
  Z = inputArguments{1};
  h = __surf__('Parent', go, 'ZData', Z, propertiesList{:});
end
%=============================================================================
function h = surfaceTwoRhs(go, inputArguments, propertiesList)
  Z = inputArguments{1};
  C = inputArguments{2};
  if (~isreal(Z) || ~isreal(C))
    error (_('X, C arguments must be real.'));
  end
  if (ismatrix(Z) && ~isvector(Z) && ~isscalar(Z))
    [nr, nc] = size(Z);
    X = 1:nc;
    Y = (1:nr)';
  else
    error (_('Z argument must be a matrix.'));
  end
  h = __surf__('Parent', go, 'ZData', Z,  'CData', C, propertiesList{:});
end
%=============================================================================
function h = surfaceThreeRhs(go, inputArguments, propertiesList)
  X = inputArguments{1};
  Y = inputArguments{2};
  Z = inputArguments{3};
  if (~isreal(X) || ~isreal(Y) || ~isreal(Z))
    error(_('X, Y, Z arguments must be real.'));
  end
  
  if (isvector(X) && isvector(Y) && ismatrix(Z))
    if (size(Z, 1) == length(Y) && size(Z, 2) == length(X))
      X = X(:)';
      Y = Y(:);
    else
      error(_('size(Z, 1) must be the same as length(Y) and size(Z, 2) must be the same as length(X).'));
    end
  elseif (ismatrix(X) && ismatrix(Y) && ismatrix(Z))
    [X_nr, X_nc] = size(X);
    [Y_nr, Y_nc] = size(Y);
    [Z_nr, Z_nc] = size(Z);
    isSameSize = ((X_nr== Y_nr) && (Y_nr == Z_nr) && (X_nc == Y_nc) && (Y_nc == Z_nc));
    if (~isSameSize)
      error(_('X, Y, and Z must have the same dimensions.'));
    end
  else
    error(_('X and Y must be vectors and Z must be a matrix.'));
  end
  h = __surf__('Parent', go, 'XData', X,  'YData', Y, 'ZData', Z, propertiesList{:});
end
%=============================================================================
function h = surfaceFourRhs(go, inputArguments, propertiesList)
  X = inputArguments{1};
  Y = inputArguments{2};
  Z = inputArguments{3};
  C = inputArguments{4};
  if (~isreal(X) || ~isreal(Y) || ~isreal(Z) || ~isreal(C))
    error(_('X, Y, Z, C arguments must be real.'));
  end
  [Z_nr, Z_nc] = size(Z);
  [C_nr, C_nc, C_np] = size(C);
  if (~(Z_nr == C_nr && Z_nc == C_nc && (C_np == 1 || C_np == 3)))
    error(_('Z and C must have the same size.'));
  end
  if (isvector(X) && isvector(Y) && ismatrix(Z))
    if (size(Z, 1) == length(Y) && size(Z, 2) == length(X))
      X = X(:)';
      Y = Y(:);
    else
      error(_('size(Z, 1) must be the same as length(Y) and size(Z, 2) must be the same as length(X).'));
    end
  elseif (ismatrix(X) && ismatrix(Y) && ismatrix(Z))
    [X_nr, X_nc] = size(X);
    [Y_nr, Y_nc] = size(Y);
    [Z_nr, Z_nc] = size(Z);
    isSameSize = ((X_nr== Y_nr) && (Y_nr == Z_nr) && (X_nc == Y_nc) && (Y_nc == Z_nc));
    if (~isSameSize)
      error(_('X, Y, and Z must have the same dimensions.'));
    end
  else
    error(_('X and Y must be vectors and Z must be a matrix.'));
  end
  h = __surf__('Parent', go, 'XData', X, 'YData', Y, 'ZData', Z, 'CData', C,  propertiesList{:});
end
%=============================================================================
