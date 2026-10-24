%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = coneplot(varargin)
  narginchk(6, 1000);
  nargoutchk(0, 1);

  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);

  ax = graphicsAddTargetAxes(parent);
  if isempty(parent)
    container = ax;
  else
    container = parent;
  end

  [field, tail] = parseConeplotArgs(args);
  [options, propertyPairs] = parseConeplotTail(tail, field.referenceSize);
  field = resolveConeplotVectors(field, options);
  colorData = resolveConeplotColors(field, options);
  coneScale = coneplotAutoscale(field, options);

  if options.useQuiver
    if options.scaleSpecified
      quiverScale = options.scale;
    else
      quiverScale = 0;
    end
    if isempty(parent)
      h = quiver3(ax, field.x, field.y, field.z, field.u, field.v, field.w, ...
        quiverScale, propertyPairs{:});
    else
      h = quiver3(parent, field.x, field.y, field.z, field.u, field.v, field.w, ...
        quiverScale, propertyPairs{:});
    end
  else
    h = renderConeplotPatch(field.x, field.y, field.z, field.u, field.v, field.w, ...
      colorData, coneScale, container, propertyPairs);
  end

  view(ax, 3);
  grid(ax, 'on');
  h.UserData = struct('ChartType', 'coneplot', 'XData', field.x, 'YData', field.y, 'ZData', field.z);

  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [field, tail] = parseConeplotArgs(args)
  firstText = find(cellfun('isclass', args, 'char'), 1);
  if isempty(firstText)
    dataCount = numel(args);
    if dataCount < 6
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
    end
    if dataCount > 6 && dataCount < 9
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
    end
    if dataCount > 9
      dataCount = 9;
    else
      dataCount = min(dataCount, 9);
    end
    dataArgs = args(1:dataCount);
    tail = args(dataCount + 1:end);
  else
    dataCount = firstText - 1;
    if dataCount < 6
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
    end
    if dataCount > 9
      error('Nelson:tooManyInputs', 'Too many input arguments.');
    end
    if dataCount ~= 6 && dataCount ~= 9
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
    end
    dataArgs = args(1:dataCount);
    tail = args(firstText:end);
  end

  switch dataCount
    case 6
      field.x = dataArgs{1};
      field.y = dataArgs{2};
      field.z = dataArgs{3};
      field.u = dataArgs{4};
      field.v = dataArgs{5};
      field.w = dataArgs{6};
      field.interpEnabled = false;
      field.interpX = [];
      field.interpY = [];
      field.interpZ = [];
      field.interpU = [];
      field.interpV = [];
      field.interpW = [];
      field.referenceSize = size(field.u);
    case 9
      field.interpX = dataArgs{1};
      field.interpY = dataArgs{2};
      field.interpZ = dataArgs{3};
      field.interpU = dataArgs{4};
      field.interpV = dataArgs{5};
      field.interpW = dataArgs{6};
      [field.interpX, field.interpY, field.interpZ] = ...
        volumeGridVectors(field.interpX, field.interpY, field.interpZ, field.interpU);
      field.x = dataArgs{7};
      field.y = dataArgs{8};
      field.z = dataArgs{9};
      field.interpEnabled = true;
      field.u = field.interpU;
      field.v = field.interpV;
      field.w = field.interpW;
      field.referenceSize = size(field.interpU);
  end

  if field.interpEnabled
    validateConeplotData(field.interpX, field.interpY, field.interpZ, ...
      field.interpU, field.interpV, field.interpW);
    if ~isempty(field.x) && ~(isequal(size(field.x), size(field.y)) ...
        && isequal(size(field.x), size(field.z)))
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
    end
  else
    validateConeplotData(field.x, field.y, field.z, field.u, field.v, field.w);
  end

  if ~(isequal(size(field.u), size(field.v)) && isequal(size(field.u), size(field.w)))
    error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
  end

end
%=============================================================================
function [options, propertyPairs] = parseConeplotTail(tail, referenceSize)
  options.scale = 1;
  options.scaleSpecified = false;
  options.method = 'linear';
  options.interpolate = true;
  options.color = [];
  options.useQuiver = false;

  k = 1;
  while k <= numel(tail)
    current = tail{k};
    if isTextScalar(current)
      key = lower(current);
      switch key
        case 'quiver'
          options.useQuiver = true;
          k = k + 1;
        case 'nointerp'
          options.interpolate = false;
          k = k + 1;
        case {'linear', 'cubic', 'nearest'}
          options.method = key;
          k = k + 1;
        otherwise
          break
      end
    elseif isscalar(current)
      options.scale = double(current);
      options.scaleSpecified = true;
      k = k + 1;
      continue
    elseif isNumericColor(current, referenceSize)
      options.color = current;
      k = k + 1;
    else
      break
    end
  end

  if mod(numel(tail) - k + 1, 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  propertyPairs = tail(k:end);
  if options.useQuiver
    return
  end
  for k = 1:2:length(propertyPairs)
    if ~isTextScalar(propertyPairs{k}) || ...
        ~ isValidGraphicsProperty('patch', propertyPairs{k})
      error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
    end
  end
end
%=============================================================================
function field = resolveConeplotVectors(field, options)
  if ~field.interpEnabled
    return
  end
  if ~options.interpolate
    if ~isequal(size(field.u), size(field.x))
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
    end
    return
  end
  field.u = interp3(field.interpX, field.interpY, field.interpZ, field.interpU, ...
    field.x, field.y, field.z, options.method, 0);
  field.v = interp3(field.interpX, field.interpY, field.interpZ, field.interpV, ...
    field.x, field.y, field.z, options.method, 0);
  field.w = interp3(field.interpX, field.interpY, field.interpZ, field.interpW, ...
    field.x, field.y, field.z, options.method, 0);
end
%=============================================================================
function colorData = resolveConeplotColors(field, options)
  colorData = [];
  if isempty(options.color)
    return
  end
  if field.interpEnabled && options.interpolate
    colorData = interp3(field.interpX, field.interpY, field.interpZ, options.color, ...
      field.x, field.y, field.z, options.method, NaN);
  else
    colorData = options.color;
  end
end
%=============================================================================
function h = renderConeplotPatch(x, y, z, u, v, w, colorData, coneScale, container, propertyPairs)
  [x, y, z, u, v, w, colorData] = vectorizeConeplotData(x, y, z, u, v, w, colorData);
  if isempty(x) || all(u == 0 & v == 0 & w == 0)
    h = patch('Parent', container, 'Faces', zeros(0, 3), 'Vertices', zeros(0, 3), propertyPairs{:});
    return
  end

  lengths = sqrt(u .^ 2 + v .^ 2 + w .^ 2);
  valid = isfinite(lengths) & (lengths > 0);
  if ~any(valid)
    h = patch('Parent', container, 'Faces', zeros(0, 3), 'Vertices', zeros(0, 3), propertyPairs{:});
    return
  end
  x = x(valid);
  y = y(valid);
  z = z(valid);
  u = u(valid);
  v = v(valid);
  w = w(valid);
  lengths = lengths(valid);
  if ~isempty(colorData)
    colorData = colorData(valid);
    if numel(colorData) ~= numel(u)
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
    end
  end

  [templateVertices, templateFaces] = coneTemplateMesh();
  [nbCones, vertexCount, faceCount] = computeConeSizes(x, templateVertices, templateFaces);
  if nbCones == 0
    h = patch('Parent', container, 'Faces', zeros(0, 3), 'Vertices', zeros(0, 3), propertyPairs{:});
    return
  end

  vertices = zeros(nbCones * vertexCount, 3);
  faces = zeros(nbCones * faceCount, 3);
  if ~isempty(colorData)
    colorPerVertex = zeros(nbCones * vertexCount, 1);
  else
    colorPerVertex = [];
  end

  coneWidth = 0.333;
  for k = 1:nbCones
    direction = [u(k), v(k), w(k)] / lengths(k);
    localHeight = lengths(k) * coneScale;
    radius = 0.5 * coneWidth * localHeight;
    localVertices = templateVertices;
    localVertices(:, 1) = localVertices(:, 1) * radius;
    localVertices(:, 2) = localVertices(:, 2) * radius;
    localVertices(:, 3) = localVertices(:, 3) * localHeight;
    localVertices = (rotationToZAxis(direction) * localVertices')' + [x(k), y(k), z(k)];

    vertexOffset = (k - 1) * vertexCount;
    faceOffset = (k - 1) * faceCount;
    vertices(vertexOffset + (1:vertexCount), :) = localVertices;
    faces(faceOffset + (1:faceCount), :) = templateFaces + vertexOffset;
    if ~isempty(colorPerVertex)
      colorPerVertex(vertexOffset + (1:vertexCount)) = colorData(k);
    end
  end

  patchArgs = {'Parent', container, 'Faces', faces, 'Vertices', vertices};
  if ~isempty(colorPerVertex)
    if ~hasProperty(propertyPairs, 'CData') && ~hasProperty(propertyPairs, 'FaceVertexCData') ...
        && ~hasProperty(propertyPairs, 'FaceColor')
      patchArgs = [patchArgs, {'FaceColor', 'interp', 'FaceVertexCData', colorPerVertex}];
    end
  end
  patchArgs = [patchArgs, propertyPairs];
  h = patch(patchArgs{:});
end
%=============================================================================
function [x, y, z, u, v, w, colorData] = vectorizeConeplotData(x, y, z, u, v, w, colorData)
  if ~(isnumeric(x) && isnumeric(y) && isnumeric(z) && isnumeric(u) && isnumeric(v) && isnumeric(w))
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
  end
  x = x(:);
  y = y(:);
  z = z(:);
  u = u(:);
  v = v(:);
  w = w(:);
  if ~isempty(colorData)
    colorData = colorData(:);
  end
end
%=============================================================================
function [nbCones, vertexCount, faceCount] = computeConeSizes(x, templateVertices, templateFaces)
  nbCones = numel(x);
  vertexCount = size(templateVertices, 1);
  faceCount = size(templateFaces, 1);
end
%=============================================================================
function [templateVertices, templateFaces] = coneTemplateMesh()
  n = 12;
  theta = linspace(0, 2 * pi, n + 1);
  circleX = cos(theta(1:n))';
  circleY = sin(theta(1:n))';
  circleZ = zeros(n, 1);
  tip = [0, 0, 1];
  center = [0, 0, 0];
  templateVertices = [circleX, circleY, circleZ; center; tip];
  templateFaces = zeros(2 * n, 3);
  for k = 1:n
    k2 = k + 1;
    if k2 > n
      k2 = 1;
    end
    templateFaces(k, :) = [n + 2, k, k2];
    templateFaces(k + n, :) = [n + 1, k2, k];
  end
end
%=============================================================================
function R = rotationToZAxis(direction)
  epsilon = 1.0e-12;
  if abs(direction(3) - 1) < epsilon
    R = eye(3);
    return
  end
  if abs(direction(3) + 1) < epsilon
    R = [-1, 0, 0; 0, -1, 0; 0, 0, 1];
    return
  end
  axis = cross([0 0 1], direction);
  s = norm(axis);
  if s < epsilon
    R = eye(3);
    return
  end
  axis = axis / s;
  c = direction(3);
  K = [0, -axis(3), axis(2); axis(3), 0, -axis(1); -axis(2), axis(1), 0];
  R = eye(3) + K + (K * K) * (1 - c) / (s ^ 2);
end
%=============================================================================
function coneScale = coneplotAutoscale(field, options)
  scaleArg = options.scale;
  if scaleArg == 0
    coneScale = 1;
    return
  end
  if field.interpEnabled && options.interpolate
    gridX = field.interpX;
    gridY = field.interpY;
    gridZ = field.interpZ;
    fieldU = field.interpU;
    fieldV = field.interpV;
    fieldW = field.interpW;
  else
    gridX = field.x;
    gridY = field.y;
    gridZ = field.z;
    fieldU = field.u;
    fieldV = field.v;
    fieldW = field.w;
  end
  sz = size(fieldU);
  nbRows = sz(1);
  if numel(sz) >= 2
    nbCols = sz(2);
  else
    nbCols = 1;
  end
  if numel(sz) >= 3
    nbPages = sz(3);
  else
    nbPages = 1;
  end
  if isempty(gridX)
    dx = nbCols - 1;
    dy = nbRows - 1;
    dz = nbPages - 1;
  else
    dx = max(gridX(:)) - min(gridX(:));
    dy = max(gridY(:)) - min(gridY(:));
    dz = max(gridZ(:)) - min(gridZ(:));
  end
  delx = dx / nbCols;
  dely = dy / nbRows;
  delz = dz / nbPages;
  del = sqrt(delx .^ 2 + dely .^ 2 + delz .^ 2);
  coneScale = scaleArg;
  if del ~= 0
    lengths = sqrt((fieldU(:) / del) .^ 2 + (fieldV(:) / del) .^ 2 + (fieldW(:) / del) .^ 2);
    maxLength = max(lengths(isfinite(lengths)));
    if isempty(maxLength) || maxLength == 0
      coneScale = 0;
    else
      coneScale = scaleArg / maxLength;
    end
  end
end
%=============================================================================
function validateConeplotData(x, y, z, u, v, w)
  values = {x, y, z, u, v, w};
  for k = 1:numel(values)
    if ~isnumeric(values{k})
      error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Value');
    end
  end
  if ~(isequal(size(u), size(v)) && isequal(size(u), size(w)))
    error('Nelson:dimagree:vectorComponentSizesMatch', 'Vector component sizes must match.');
  end
  validateConeplotCoordinateSize(x, size(u), 'X');
  validateConeplotCoordinateSize(y, size(u), 'Y');
  validateConeplotCoordinateSize(z, size(u), 'Z');
end
%=============================================================================
function validateConeplotCoordinateSize(value, targetSize, axisName)
  if isscalar(value) || isequal(size(value), targetSize)
    return
  end
  if isvector(value) && (numel(value) == targetSize(1) || ...
      numel(value) == targetSize(2) || numel(value) == prod(targetSize))
    return
  end
  error('Nelson:graphics:componentDataSizeMismatch', '%s must match vector component size.', axisName);
end
%=============================================================================
function tf = isNumericColor(value, referenceSize)
  tf = false;
  if ~isnumeric(value)
    return
  end
  if isscalar(value)
    return
  end
  if isequal(size(value), referenceSize)
    tf = true;
    return
  end
  if numel(value) == prod(referenceSize)
    tf = true;
  end
end
%=============================================================================
function has = hasProperty(propertyPairs, name)
  has = false;
  if isempty(propertyPairs)
    return
  end
  for k = 1:2:length(propertyPairs)
    if isTextScalar(propertyPairs{k}) && strcmpi(propertyPairs{k}, name)
      has = true;
      return
    end
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
%=============================================================================
