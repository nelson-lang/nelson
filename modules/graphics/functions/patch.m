%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = patch(varargin)
  % h = patch(X, Y, C)
  % h = patch(X, Y, Z, C)
  % h = patch(ax, X, Y, C)
  % h = patch(ax, X, Y, Z, C)

  nargoutchk(0, 1);

  inputArguments = convertStringToCharArgs(varargin);
  inputArguments = expandPatchStructArgument(inputArguments);
  [h, isFastPatch] = renderPatchFast(inputArguments);
  if ~isFastPatch
    [h, isFastPatch] = renderPatchPropertyFast(inputArguments);
  end
  if isFastPatch
    if (nargout > 0)
      varargout{1} = h;
    else
      varargout = {};
    end
    return
  end

  args = parsePatchArguments(inputArguments);
  h = renderPatch(args);
  updatePatchLimits(struct(args{:}));

  if (nargout > 0)
    varargout{1} = h;
  else
    varargout = {};
  end
end
%=============================================================================
function [h, isFastPatch] = renderPatchFast(inputArguments)
  h = [];
  isFastPatch = false;
  if isempty(inputArguments)
    return
  end
  if length(inputArguments) == 1 && isstruct(inputArguments{1})
    return
  end
  if isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') || isgraphics(inputArguments{1}, 'hgtransform'))
    return
  end
  if length(inputArguments) < 3
    return
  end
  if ~isnumeric(inputArguments{1}) || ~isnumeric(inputArguments{2})
    return
  end
  if ndims(inputArguments{1}) > 2 || ndims(inputArguments{2}) > 2
    return
  end
  [dataCount, propertyStart] = fastPatchDataCount(inputArguments);
  if dataCount == 0
    return
  end
  for k = 1:dataCount
    if isnumeric(inputArguments{k}) && ndims(inputArguments{k}) > 2
      return
    end
  end
  if hasFastPatchDataOverride(inputArguments, propertyStart)
    return
  end
  h = __patch__(inputArguments{:});
  isFastPatch = true;
end
%=============================================================================
function [h, isFastPatch] = renderPatchPropertyFast(inputArguments)
  h = [];
  isFastPatch = false;
  if isempty(inputArguments)
    return
  end
  if length(inputArguments) == 1 && isstruct(inputArguments{1})
    return
  end
  parent = [];
  if isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') || isgraphics(inputArguments{1}, 'hgtransform'))
    parent = inputArguments{1};
    inputArguments = inputArguments(2:end);
  end
  if isempty(inputArguments) || rem(length(inputArguments), 2) ~= 0
    return
  end
  for k = 1:2:length(inputArguments)
    if ~isFastPatchPropertyPairName(inputArguments{k})
      return
    end
  end
  if ~hasPatchProperty(inputArguments, 'Faces') || ~hasPatchProperty(inputArguments, 'Vertices')
    return
  end
  if ~isDirectPatchPropertyData(inputArguments)
    return
  end
  if ~isempty(parent) && ~hasPatchProperty(inputArguments, 'Parent')
    inputArguments = [{'Parent', parent}, inputArguments];
  end
  h = __patch__(inputArguments{:});
  isFastPatch = true;
end
%=============================================================================
function tf = isFastPatchPropertyPairName(value)
  if ~ischar(value)
    tf = false;
    return
  end
  persistent propertyNames
  if isempty(propertyNames)
    propertyNames = {'alignvertexcenters', 'alphadatamapping', 'ambientstrength', ...
      'annotation', 'backfacelighting', 'beingdeleted', 'busyaction', ...
      'buttondownfcn', 'children', 'clipping', 'contextmenu', 'createfcn', ...
      'cdatamapping', 'cdata', 'deletefcn', 'diffusestrength', 'displayname', ...
      'edgealpha', 'edgecolor', 'edgelighting', 'facelighting', 'facealpha', ...
      'facecolor', 'faces', 'facevertexalphadata', 'facevertexcdata', ...
      'handlevisibility', 'hittest', 'interruptible', 'linestyle', 'linewidth', ...
      'linejoin', 'marker', 'markeredgealpha', 'markeredgecolor', 'markerfacealpha', ...
      'markerfacecolor', 'markersize', 'parent', 'pickableparts', 'selected', ...
      'selectionhighlight', 'seriesindex', 'specularcolorreflectance', ...
      'specularexponent', 'specularstrength', 'tag', 'type', 'userdata', ...
      'vertices', 'visible', 'xdata', 'ydata', 'zdata'};
  end
  tf = any(strcmpi(value, propertyNames));
end
%=============================================================================
function tf = isDirectPatchPropertyData(propertiesList)
  tf = true;
  faces = getPatchPropertyValue(propertiesList, 'Faces');
  vertices = getPatchPropertyValue(propertiesList, 'Vertices');
  if ~isnumeric(faces) || ~isnumeric(vertices) || size(vertices, 2) ~= 3
    tf = false;
    return
  end
  cdata = getPatchPropertyValue(propertiesList, 'FaceVertexCData');
  if isempty(cdata) || ~isnumeric(cdata)
    return
  end
  faceColor = getPatchPropertyValue(propertiesList, 'FaceColor');
  if isempty(faceColor) || isnumeric(faceColor)
    return
  end
  if strcmpi(faceColor, 'flat')
    tf = ismatrix(cdata) && size(cdata, 2) == 3 && size(cdata, 1) == size(faces, 1);
    return
  end
  if strcmpi(faceColor, 'interp')
    % The renderer maps per-vertex colour data directly: an Nx3 RGB matrix or
    % an Nx1 scalar column (one value per vertex) mapped through the colormap.
    % Both need one row per vertex.
    tf = ismatrix(cdata) && (size(cdata, 2) == 3 || size(cdata, 2) == 1) ...
      && size(cdata, 1) == size(vertices, 1);
  end
end
%=============================================================================
function value = getPatchPropertyValue(propertiesList, name)
  value = [];
  for k = 1:2:length(propertiesList)
    if ischar(propertiesList{k}) && strcmpi(propertiesList{k}, name)
      value = propertiesList{k + 1};
      return
    end
  end
end
%=============================================================================
function [dataCount, propertyStart] = fastPatchDataCount(inputArguments)
  dataCount = 0;
  propertyStart = length(inputArguments) + 1;
  nbInputArguments = length(inputArguments);
  if nbInputArguments < 3
    return
  end
  if isFastFaceColorSpec(inputArguments{3})
    dataCount = 2;
    propertyStart = 4;
    return
  end
  if isFastPatchPropertyName(inputArguments{3})
    return
  end
  if nbInputArguments == 3
    dataCount = 3;
    return
  end
  if isFastFaceColorSpec(inputArguments{4})
    dataCount = 3;
    propertyStart = 5;
    return
  end
  if isFastPatchPropertyName(inputArguments{4})
    dataCount = 3;
    propertyStart = 4;
    return
  end
  dataCount = 4;
  propertyStart = 5;
end
%=============================================================================
function tf = hasFastPatchDataOverride(inputArguments, propertyStart)
  tf = true;
  if propertyStart > length(inputArguments)
    tf = false;
    return
  end
  if rem(length(inputArguments) - propertyStart + 1, 2) ~= 0
    return
  end
  for k = propertyStart:2:length(inputArguments)
    if ~isFastPatchPropertyName(inputArguments{k})
      return
    end
    name = lower(inputArguments{k});
    if any(strcmp(name, {'parent', 'xdata', 'ydata', 'zdata', 'cdata', 'faces', 'vertices', 'facevertexcdata'}))
      return
    end
  end
  tf = false;
end
%=============================================================================
function tf = isFastFaceColorSpec(value)
  tf = (ischar(value) && ~isFastPatchPropertyName(value)) ...
    || (isnumeric(value) && isvector(value) && numel(value) == 3);
end
%=============================================================================
function tf = isFastPatchPropertyName(value)
  if ~ischar(value)
    tf = false;
    return
  end
  persistent propertyNames
  if isempty(propertyNames)
    propertyNames = {'alignvertexcenters', 'alphadatamapping', 'ambientstrength', ...
      'annotation', 'backfacelighting', 'beingdeleted', 'busyaction', ...
      'buttondownfcn', 'children', 'clipping', 'contextmenu', 'createfcn', ...
      'cdatamapping', 'deletefcn', 'diffusestrength', 'displayname', 'edgealpha', ...
      'edgecolor', 'edgelighting', 'facelighting', 'facealpha', 'facecolor', ...
      'handlevisibility', 'hittest', 'interruptible', 'linestyle', 'linewidth', ...
      'linejoin', 'marker', 'markeredgealpha', 'markeredgecolor', 'markerfacealpha', ...
      'markerfacecolor', 'markersize', 'pickableparts', 'selected', ...
      'selectionhighlight', 'seriesindex', 'specularcolorreflectance', ...
      'specularexponent', 'specularstrength', 'tag', 'type', 'userdata', 'visible'};
  end
  tf = any(strcmpi(value, propertyNames));
end
%=============================================================================
function updatePatchLimits(st)
  if ~isfield(st, 'Parent') || ~isgraphics(st.Parent, 'axes')
    return
  end
  if ~isfield(st, 'Vertices') || isempty(st.Vertices)
    return
  end
  ax = st.Parent;
  vertices = st.Vertices;
  expandLimits(ax, vertices(:, 1), 'X');
  expandLimits(ax, vertices(:, 2), 'Y');
  if size(vertices, 2) >= 3 && any(vertices(:, 3) ~= vertices(1, 3))
    expandLimits(ax, vertices(:, 3), 'Z');
  end
end
%=============================================================================
function expandLimits(ax, values, axisName)
  % When the axis limit mode is 'auto', the axis already recomputes padded
  % limits from all of its children. Overwriting the limits here would freeze
  % the mode to 'manual' and drop that padding, making the plotted data fill
  % the whole box (appearing larger than expected). Only adjust the limits
  % when the user has already fixed them ('manual'), so newly added data stays
  % visible without discarding an auto (padded) range.
  if strcmp(ax.([axisName, 'LimMode']), 'auto')
    return
  end
  values = values(isfinite(values));
  if isempty(values)
    return
  end
  dataRange = [min(values), max(values)];
  if dataRange(1) == dataRange(2)
    dataRange(2) = dataRange(1) + 1;
  end
  currentRange = ax.([axisName, 'Lim']);
  dataRange = [min(currentRange(1), dataRange(1)), max(currentRange(2), dataRange(2))];
  ax.([axisName, 'Lim']) = dataRange;
end
%=============================================================================
function h = renderPatch(args)
  st = struct(args{:});
  if requires3DNumericFlatColorConversion(st)
    st.FaceVertexCData = resolveFaceColors(st);
    st.FaceColor = 'flat';
    args = structToArgs(st);
    h = __patch__(args{:});
  else
    % Interp colouring of a 3D patch with indexed (scalar) FaceVertexCData is
    % rendered directly as a single patch: the renderer maps each vertex value
    % through the colormap and interpolates. (It used to be split into one flat
    % patch per face, which was slow -- thousands of objects for a surf2patch --
    % and only approximated the interpolation.)
    h = __patch__(args{:});
  end
end
%=============================================================================
function tf = requires3DNumericFlatColorConversion(st)
  tf = requires3DNumericColorFallback(st) && strcmpi(st.FaceColor, 'flat');
end
%=============================================================================
function tf = requires3DNumericColorFallback(st)
  tf = false;
  if ~isfield(st, 'Vertices') || isempty(st.Vertices) || size(st.Vertices, 2) < 3
    return
  end
  z = st.Vertices(:, 3);
  if isempty(z) || all(z == z(1))
    return
  end
  if ~isfield(st, 'FaceVertexCData') || isempty(st.FaceVertexCData) || ~isnumeric(st.FaceVertexCData)
    return
  end
  if ~isfield(st, 'FaceColor')
    return
  end
  if isnumeric(st.FaceColor)
    return
  end
  if strcmpi(st.FaceColor, 'flat') && isDirectFlatFaceColorData(st)
    return
  end
  if strcmpi(st.FaceColor, 'interp') && isDirectInterpVertexColorData(st)
    return
  end
  tf = strcmpi(st.FaceColor, 'flat') || strcmpi(st.FaceColor, 'interp');
end
%=============================================================================
function tf = isDirectFlatFaceColorData(st)
  c = st.FaceVertexCData;
  tf = ismatrix(c) && size(c, 2) == 3 && isfield(st, 'Faces') && size(c, 1) == size(st.Faces, 1);
end
%=============================================================================
function tf = isDirectInterpVertexColorData(st)
  % The renderer interpolates per-vertex colours itself when FaceVertexCData has
  % one row per vertex: either an Nx3 RGB matrix, or an Nx1 scalar column mapped
  % through the colormap (e.g. surf2patch output). Both render as a single patch,
  % so the per-face fallback is unnecessary for them.
  c = st.FaceVertexCData;
  tf = ismatrix(c) && (size(c, 2) == 3 || size(c, 2) == 1) ...
    && isfield(st, 'Vertices') && size(c, 1) == size(st.Vertices, 1);
end
%=============================================================================
function args = expandPatchStructArgument(args)
  % patch(S, Name, Value, ...) and patch(ax, S, Name, Value, ...): expand a
  % faces/vertices struct (e.g. from surf2patch) into Name/Value pairs so the
  % trailing properties are honoured. A lone struct argument is left for the
  % downstream struct handling.
  structIndex = 0;
  if ~isempty(args) && isstruct(args{1})
    structIndex = 1;
  elseif numel(args) >= 2 && isscalar(args{1}) && isgraphics(args{1}) && isstruct(args{2})
    structIndex = 2;
  end
  if structIndex == 0 || numel(args) == structIndex
    return
  end
  st = args{structIndex};
  structArgs = reshape([fieldnames(st)'; struct2cell(st)'], 1, []);
  args = [args(1:structIndex - 1), structArgs, args(structIndex + 1:end)];
end
%=============================================================================
function args = structToArgs(st)
  args = reshape([fieldnames(st)'; struct2cell(st)'], 1, []);
end
%=============================================================================
function colors = resolveFaceColors(st)
  c = st.FaceVertexCData;
  faces = st.Faces;
  vertices = st.Vertices;
  if ndims(c) == 3 && size(c, 3) == 3
    rgb = reshape(c, [], 3);
    colors = zeros(size(faces, 1), 3);
    for k = 1:size(faces, 1)
      colors(k, :) = mean(rgb(faces(k, :), :), 1);
    end
    return
  end
  if ismatrix(c) && size(c, 2) == 3 && size(c, 1) == size(vertices, 1)
    colors = zeros(size(faces, 1), 3);
    for k = 1:size(faces, 1)
      colors(k, :) = mean(c(faces(k, :), :), 1);
    end
    return
  end
  values = faceScalarValues(st);
  colors = mapScalarsToColors(st.Parent, values);
end
%=============================================================================
function values = faceScalarValues(st)
  c = st.FaceVertexCData;
  faces = st.Faces;
  if isscalar(c)
    values = repmat(c, size(faces, 1), 1);
    return
  end
  if isvector(c)
    c = c(:);
    values = zeros(size(faces, 1), 1);
    for k = 1:size(faces, 1)
      values(k) = mean(c(faces(k, :)));
    end
    return
  end
  if isequal(size(c), size(st.XData))
    values = mean(c, 1).';
    return
  end
  if isequal(size(c), fliplr(size(st.XData)))
    values = mean(c.', 1).';
    return
  end
  values = mean(c, 1).';
end
%=============================================================================
function colors = mapScalarsToColors(parent, values)
  colors = colormap(parent);
  if isempty(colors)
    colors = colormap();
  end
  if isempty(colors)
    colors = jet(64);
  end
  values = values(:);
  minValue = min(values);
  maxValue = max(values);
  if minValue == maxValue
    maxValue = minValue + 1;
  end
  parent.CLim = [minValue, maxValue];
  nbColors = size(colors, 1);
  index = ceil((values - minValue) ./ (maxValue - minValue) .* nbColors);
  index(index < 1) = 1;
  index(index > nbColors) = nbColors;
  colors = colors(index, :);
end
%=============================================================================
function args = parsePatchArguments(inputArguments)
  if length(inputArguments) == 1
    % patch(S)
    if isstruct(inputArguments{1})
      S = inputArguments{1};
      inputArguments = reshape([fieldnames(S)'; struct2cell(S)'], 1, []);
    else
      error('Nelson:patch:structTypeExpected', 'struct type expected.');
    end
  end

  ax = [];
  if (length(inputArguments) > 0 && isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') || isgraphics(inputArguments{1}, 'hgtransform')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  end

  nbInputArguments = length(inputArguments);
  if nbInputArguments == 0
    inputArguments = {'XData', [0; 1; 0], 'YData', [1; 1; 0]};
    nbInputArguments = length(inputArguments);
  end
  explicitFaceColor = [];

  propertyStart = nbInputArguments + 1;
  for k = 1:nbInputArguments
    if isPatchPropertyName(inputArguments{k})
      propertyStart = k;
      break
    end
  end
  propertiesList = inputArguments(propertyStart:end);
  inputArguments = inputArguments(1:propertyStart - 1);
  propertiesList = graphicsCanonicalPropertyNames(propertiesList, 'patch');
  propertiesList = graphicsCollapseRepeatedProperties(propertiesList);

  if ~hasPatchProperty(propertiesList, 'FaceColor')
    if length(inputArguments) == 3 && isFaceColorSpec(inputArguments{3})
      explicitFaceColor = parseFaceColor(inputArguments{3});
      inputArguments = inputArguments(1:2);
    elseif length(inputArguments) == 4 && isFaceColorSpec(inputArguments{4})
      explicitFaceColor = parseFaceColor(inputArguments{4});
      inputArguments = inputArguments(1:3);
    end
  end
  if ~isempty(explicitFaceColor)
    propertiesList = [{'FaceColor', explicitFaceColor}, propertiesList];
  end

  nbInputArguments = length(inputArguments);
  XData = [];
  YData = [];
  ZData = [];
  CData = [];
  haveCData = false;
  Faces = [];
  Vertices = [];
  FaceVertexCData = [];
  % Default face/edge is the dark-gray axes foreground (33/255), matching the
  % factory patch defaults -- not pure black.
  FaceColorDefault = [0.129411764705882, 0.129411764705882, 0.129411764705882];
  EdgeColorDefault = [0.129411764705882, 0.129411764705882, 0.129411764705882];

  if (nbInputArguments == 2 && ~isempty(explicitFaceColor))
    XData = inputArguments{1};
    YData = inputArguments{2};
  elseif (nbInputArguments == 3 && isempty(explicitFaceColor))
    % h = patch(X, Y, C)
    XData = inputArguments{1};
    YData = inputArguments{2};
    CData = inputArguments{3};
    haveCData = true;
  elseif (nbInputArguments == 3 && ~isempty(explicitFaceColor))
    XData = inputArguments{1};
    YData = inputArguments{2};
    ZData = inputArguments{3};
  elseif (nbInputArguments == 4)
    % h = patch(X, Y, Z, C)
    XData = inputArguments{1};
    YData = inputArguments{2};
    ZData = inputArguments{3};
    CData = inputArguments{4};
    haveCData = true;
  elseif (nbInputArguments > 4)
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  elseif (nbInputArguments > 0)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end


  if (isvector(XData))
    XData = XData(:);
    YData = YData(:);
    ZData = ZData(:);
    if (haveCData && isnumeric(CData))
      if (isvector(CData) && numel(CData) == numel(XData))
        CData = CData(:);
      elseif (size(CData, 1) ~= numel(XData) && size(CData, 2) == numel(XData))
        CData = CData.';
      end
    end
  end

  args = struct(propertiesList{:});
  ParentAsProperty = getValueFromStruct(args, 'Parent');
  if isfield(args, 'Parent')
    rmfield(args, 'Parent');
  end

  XDataAsProperty = getValueFromStruct(args, 'XData');
  if isfield(args, 'XData')
    rmfield(args, 'XData');
  end

  YDataAsProperty = getValueFromStruct(args, 'YData');
  if isfield(args, 'YData')
    rmfield(args, 'YData');
  end

  ZDataAsProperty = getValueFromStruct(args, 'ZData');
  if isfield(args, 'ZData')
    rmfield(args, 'ZData');
  end

  CDataAsProperty = getValueFromStruct(args, 'CData');
  if isfield(args, 'CData')
    rmfield(args, 'CData');
  end

  FacesAsProperty = getValueFromStruct(args, 'Faces');
  if isfield(args, 'Faces')
    rmfield(args, 'Faces');
  end

  FaceAlphasAsProperty = getValueFromStruct(args, 'FaceAlpha');
  if isfield(args, 'FaceAlpha')
    rmfield(args, 'FaceAlpha');
  end

  VerticesAsProperty = getValueFromStruct(args, 'Vertices');
  if isfield(args, 'Vertices')
    rmfield(args, 'Vertices');
  end

  FaceVertexCDataAsProperty = getValueFromStruct(args, 'FaceVertexCData');
  if isfield(args, 'FaceVertexCData')
    rmfield(args, 'FaceVertexCData');
  end

  FaceColorAsProperty = getValueFromStruct(args, 'FaceColor');
  if isfield(args, 'FaceColor')
    rmfield(args, 'FaceColor');
  end

  EdgeColorAsProperty = getValueFromStruct(args, 'EdgeColor');
  if isfield(args, 'EdgeColor')
    rmfield(args, 'EdgeColor');
  end

  CDataMappingAsProperty = getValueFromStruct(args, 'CDataMapping');
  if isfield(args, 'CDataMapping')
    rmfield(args, 'CDataMapping');
  end

  if isempty(CDataMappingAsProperty)
    CDataMapping = 'scaled';
  else
    CDataMapping = CDataMappingAsProperty;
  end

  if isempty(ParentAsProperty)
    if isempty(ax)
      Parent = gca();
    else
      Parent = ax;
    end
  else
    Parent = ParentAsProperty;
  end
  colorAxes = Parent;
  if isgraphics(Parent, 'hggroup') || isgraphics(Parent, 'hgtransform')
    colorAxes = ancestor(Parent, 'axes');
  end

  if (haveCData && isvector(CData) && length(CData) ~= 3)
    colors = colormap(colorAxes);
    nbColors = size(colors, 1);
    if strcmp(CDataMapping, 'scaled')
      CLim = colorAxes.CLim;
      index = ceil((CData - CLim(1)) / (CLim(2) - CLim(1)) * (nbColors));
      wrap = mod(index(index > nbColors), nbColors) + 1;
      if ~isempty(wrap)
        index(index > nbColors) = wrap;
      end
      index(index < 1) = 1;
      CData = colors(index, :);
    elseif strcmp(CDataMapping, 'direct')
      CData = CData;
    else
      error('Nelson:patch:invalidCDataMapping', 'CDataMapping scaled or direct value expected.');
    end
  end

  if ~isempty(XDataAsProperty)
    XData = XDataAsProperty;
  end

  if ~isempty(YDataAsProperty)
    YData = YDataAsProperty;
  end

  if ~isempty(ZDataAsProperty)
    ZData = ZDataAsProperty;
  end

  if ~isempty(CDataAsProperty)
    CData = CDataAsProperty;
  end

  if isempty(VerticesAsProperty)
    if isempty(ZData)
      ZData = ones(size(XData));
    end
    if (size(XData) ~= size(YData) | size(XData) ~= size(ZData) | length(size(XData)) > 2)
      error('Nelson:dimagree:sizeDataMismatch', 'Size of data mismatch.');
    end
    Vertices = [XData(:), YData(:), ZData(:)];
  else
    Vertices = VerticesAsProperty;
    if isnumeric(Vertices)
      rows = size(Vertices, 1);
      cols = size(Vertices, 2);
      if (cols == 2)
        Vertices(:, 3) = ones(rows, 1);
      end
    end
  end

  if ~isempty(FacesAsProperty)
    Faces = FacesAsProperty;
  else
    [nx, ny] = size(XData);
    if (nx == 1)
      Faces = reshape(1:nx * ny, [ny, nx])';
    else
      Faces = reshape(1:nx * ny, size(XData))';
    end
  end

  if ~isempty(CDataAsProperty)
    CData = CDataAsProperty;
  else
    if ~haveCData
      CData = colorAxes.Color;
    end
  end

  if ~isempty(FaceVertexCDataAsProperty)
    FaceVertexCData = FaceVertexCDataAsProperty;
  else
    if isnumeric(CData)
      FaceVertexCData = CData;
    elseif ischar(CData)
      if strcmp(CData, 'none')
        FaceVertexCData = CData;
      else
        FaceVertexCData = validatecolor(CData);
      end
    else
      error('Nelson:patch:stringColorOrExpected', 'String color or numeric value expected.'),
    end
  end

  if ~isempty(FaceColorAsProperty)
    FaceColor = FaceColorAsProperty;
  else
    if ~haveCData
      FaceColor = FaceColorDefault;
    else
      FaceColor = 'flat';
    end
  end

  if ~isempty(EdgeColorAsProperty)
    EdgeColor = EdgeColorAsProperty;
  else
    EdgeColor = EdgeColorDefault;
  end

  if ~isempty(FaceAlphasAsProperty)
    FaceAlpha = FaceAlphasAsProperty;
  else
    FaceAlpha = 1;
  end

  if (isvector(XData) && (length(XData) == 3))
    XData = [XData(1), XData(:)']';
    YData = [YData(1), YData(:)']';
    ZData = [ZData(1), ZData(:)']';
  end

  args.Parent = Parent;
  args.XData = XData;
  args.YData = YData;
  args.ZData = ZData;
  args.CData = CData;
  args.Faces = Faces;
  args.Vertices = Vertices;
  args.FaceVertexCData = FaceVertexCData;
  args.FaceColor = FaceColor;
  args.EdgeColor = EdgeColor;
  args.FaceAlpha = FaceAlpha;

  args = reshape([fieldnames(args)'; struct2cell(args)'], 1, []);

end
%=============================================================================
function color = parseFaceColor(value)
  if isnumeric(value)
    color = value(:).';
  elseif strcmp(value, 'none')
    color = value;
  else
    color = validatecolor(value);
  end
end
%=============================================================================
function tf = isPatchPropertyName(value)
  tf = graphicsIsPropertyNameToken(value, 'patch');
end
%=============================================================================
function tf = isFaceColorSpec(value)
  tf = (ischar(value) && ~isPatchPropertyName(value)) ...
    || (isnumeric(value) && isvector(value) && numel(value) == 3);
end
%=============================================================================
function tf = hasPatchProperty(propertiesList, name)
  tf = false;
  for k = 1:2:length(propertiesList)
    if ischar(propertiesList{k}) && strcmpi(propertiesList{k}, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function value = getValueFromStruct(st, name)
  value = [];
  if isfield(st, name)
    value = st.(name);
  end
end
%=============================================================================
