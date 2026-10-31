%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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

  inputArguments = convertStringToCharArgs(varargin);
  validateSurfacePropertySyntax(inputArguments);
  nbInputArguments = nargin;
  if (nbInputArguments >= 2)
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') || isgraphics(inputArguments{1}, 'hgtransform')))
      go = inputArguments{1}(1);
      inputArguments = inputArguments(2:end);
      nbInputArguments = length(inputArguments);
    else
      go = gca();
    end
  else
    go = gca();
  end
  propertiesList = {};
  firstString = find(cellfun('isclass', inputArguments, 'char'), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end
  propertiesList = inputArguments(firstString:end);
  if ~hasSurfaceProperty(propertiesList, 'EdgeColor')
    propertiesList = [{'EdgeColor', graphicsDefaultEdgeColor()}, propertiesList];
  end
  inputArguments = inputArguments(1:firstString - 1);
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
    error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
  end
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function h = surfaceNoRhs(go, propertiesList)
  Z = eye(3, 2);
  h = __surf__('Parent', go, 'ZData', Z, propertiesList{:});
  xlim(go, [1 3]);
  ylim(go, [1 3]);
end
%=============================================================================
function h = surfaceOneRhs(go, inputArguments, propertiesList)
  Z = inputArguments{1};
  validateZData(Z);
  h = __surf__('Parent', go, 'ZData', Z, propertiesList{:});
end
%=============================================================================
function h = surfaceTwoRhs(go, inputArguments, propertiesList)
  Z = inputArguments{1};
  C = inputArguments{2};
  [Z, C] = dropImaginaryParts(Z, C);
  validateZData(Z);
  validateColorData(Z, C, isTextureFaceColor(propertiesList));
  h = __surf__('Parent', go, 'ZData', Z, 'CData', C, propertiesList{:});
end
%=============================================================================
function h = surfaceThreeRhs(go, inputArguments, propertiesList)
  X = inputArguments{1};
  Y = inputArguments{2};
  Z = inputArguments{3};
  [X, Y, Z] = dropImaginaryParts(X, Y, Z);
  validateZData(Z);
  if (isvector(X) && isvector(Y) && ismatrix(Z))
    if (size(Z, 1) == length(Y) && size(Z, 2) == length(X))
      X = X(:)';
      Y = Y(:);
    else
      error('Nelson:surface:invalidSizeZ1', 'size(Z, 1) must be the same as length(Y) and size(Z, 2) must be the same as length(X).');
    end
  elseif (ismatrix(X) && ismatrix(Y) && ismatrix(Z))
    [X_nr, X_nc] = size(X);
    [Y_nr, Y_nc] = size(Y);
    [Z_nr, Z_nc] = size(Z);
    isSameSize = ((X_nr == Y_nr) && (Y_nr == Z_nr) && (X_nc == Y_nc) && (Y_nc == Z_nc));
    if (~isSameSize)
      error('Nelson:surface:invalidXYAnd', 'X, Y, and Z must have the same dimensions.');
    end
  else
    error('Nelson:surface:invalidXAndY', 'X and Y must be vectors and Z must be a matrix.');
  end
  h = __surf__('Parent', go, 'XData', X, 'YData', Y, 'ZData', Z, propertiesList{:});
end
%=============================================================================
function h = surfaceFourRhs(go, inputArguments, propertiesList)
  X = inputArguments{1};
  Y = inputArguments{2};
  Z = inputArguments{3};
  C = inputArguments{4};
  if (isempty(C))
    % surf(X, Y, Z, []) colours by Z, like surf(X, Y, Z); the empty placeholder
    % is commonly used to reach trailing name-value pairs.
    C = Z;
  end
  [X, Y, Z, C] = dropImaginaryParts(X, Y, Z, C);
  validateZData(Z);
  validateColorData(Z, C, isTextureFaceColor(propertiesList));
  if (isvector(X) && isvector(Y) && ismatrix(Z))
    if (size(Z, 1) == length(Y) && size(Z, 2) == length(X))
      X = X(:)';
      Y = Y(:);
    else
      error('Nelson:surface:invalidSizeZ1', 'size(Z, 1) must be the same as length(Y) and size(Z, 2) must be the same as length(X).');
    end
  elseif (ismatrix(X) && ismatrix(Y) && ismatrix(Z))
    [X_nr, X_nc] = size(X);
    [Y_nr, Y_nc] = size(Y);
    [Z_nr, Z_nc] = size(Z);
    isSameSize = ((X_nr == Y_nr) && (Y_nr == Z_nr) && (X_nc == Y_nc) && (Y_nc == Z_nc));
    if (~isSameSize)
      error('Nelson:surface:invalidXYAnd', 'X, Y, and Z must have the same dimensions.');
    end
  else
    error('Nelson:surface:invalidXAndY', 'X and Y must be vectors and Z must be a matrix.');
  end
  h = __surf__('Parent', go, 'XData', X, 'YData', Y, 'ZData', Z, 'CData', C, propertiesList{:});
end
%=============================================================================
function validateZData(Z)
  if ~ismatrix(Z) || isvector(Z) || isscalar(Z) || any(size(Z) < 2)
    error('Nelson:surface:invalidZArgument', 'Z argument must be a matrix.');
  end
end
%=============================================================================
function varargout = dropImaginaryParts(varargin)
  % Complex coordinate or color data is drawn using its real part, with a
  % warning, instead of being rejected.
  anyComplex = false;
  varargout = cell(1, nargin);
  for k = 1:nargin
    value = varargin{k};
    if ~isreal(value)
      anyComplex = true;
      value = real(value);
    end
    varargout{k} = value;
  end
  if anyComplex
    warning('Nelson:surface:imaginaryPartsIgnored', 'Imaginary parts of complex arguments ignored.');
  end
end
%=============================================================================
function validateColorData(Z, C, isTexture)
  if nargin < 3
    isTexture = false;
  end
  [Z_nr, Z_nc] = size(Z);
  [C_nr, C_nc, C_np] = size(C);
  if isTexture
    % 'texturemap' FaceColor maps an image of arbitrary size onto the surface,
    % so CData only needs to be a non-empty indexed (m-by-n) or truecolor
    % (m-by-n-by-3) image; its extent is independent of Z.
    if isempty(C) || ~(C_np == 1 || C_np == 3)
      error('Nelson:dimagree:zCSize', 'CData must be an image for texture mapping.');
    end
    return
  end
  sameVertexSize = (Z_nr == C_nr && Z_nc == C_nc);
  sameFaceSize = (Z_nr > 1 && Z_nc > 1 && C_nr == Z_nr - 1 && C_nc == Z_nc - 1);
  if ~((sameVertexSize || sameFaceSize) && (C_np == 1 || C_np == 3))
    error('Nelson:dimagree:zCSize', 'Z and C must have the same size.');
  end
end
%=============================================================================
function tf = isTextureFaceColor(propertiesList)
  tf = false;
  for k = 1:2:length(propertiesList)
    if ischar(propertiesList{k}) && strcmpi(propertiesList{k}, 'FaceColor')
      value = propertiesList{k + 1};
      if (ischar(value) || isStringScalar(value))
        value = convertStringsToChars(value);
        % Any non-empty leading substring of 'texturemap' selects it, matching the
        % unambiguous-prefix acceptance of the surface FaceColor property
        % dictionary (no other value starts with 't').
        if ~isempty(value) && strncmpi(value, 'texturemap', length(value))
          tf = true;
        end
      end
      return
    end
  end
end
%=============================================================================
function tf = hasSurfaceProperty(propertiesList, propertyName)
  tf = false;
  for k = 1:2:length(propertiesList)
    if ischar(propertiesList{k}) && strcmpi(propertiesList{k}, propertyName)
      tf = true;
      return
    end
  end
end
%=============================================================================
