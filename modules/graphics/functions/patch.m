%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
  
  args = parsePatchArguments(varargin);
  
  h = __patch__(args{:});
  
  if (nargout > 0)
    varargout{1} = h;
  else
    varargout = {};
  end
end
%=============================================================================
function args = parsePatchArguments(inputArguments)
  if length(inputArguments) == 1
    % patch(S)
    if isstruct(inputArguments{1})
      S = inputArguments{1};
      inputArguments = reshape([fieldnames(S)'; struct2cell(S)'], 1, []);      
    else
      error(_('struct type expected.'));
    end
  end
  
  if (isgraphics(inputArguments{1}, 'axes'))
    ax = inputArguments{1};
    inputArguments = inputArguments{2:end};
  end

  nbInputArguments = length(inputArguments);
  if nbInputArguments == 0
    inputArguments = {'XData', [0; 1; 0], 'YData', [1; 1; 0]};
    nbInputArguments = length(inputArguments);
  end

  firstString = find (cellfun ('isclass', inputArguments, 'char'), 1);
  if (isempty(firstString))
    firstString = nbInputArguments + 1;
  end

  if ((nbInputArguments >= 3) && (firstString == 3)) || ((nbInputArguments >= 4) && (firstString == 4))
    % h = patch(X, Y, C) or h = patch(X, Y, Z, C)
    str = inputArguments{firstString};
    if ischar(str)
      if isValidGraphicsProperty('patch', str)
        propertiesList = inputArguments(firstString:end);
        inputArguments = inputArguments(1:firstString-1);
      else
        if strcmp(str, 'none')
          C = str;
        else
          C = validatecolor(str);
        end
        inputArguments = [inputArguments(1:firstString-1), C];
        propertiesList = inputArguments(firstString+1:end);
      end
    else
      propertiesList = inputArguments(firstString:end);
      inputArguments = inputArguments(1:firstString-1);
    end
  else
    propertiesList = inputArguments(firstString:end);
    inputArguments = inputArguments(1:firstString-1);
  end

  nbInputArguments = length(inputArguments);
  args = {};
  ax = [];
  XData = [];
  YData = [];
  ZData = [];
  CData = [];
  haveCData = false;
  Faces = [];
  Vertices = [];
  FaceVertexCData = [];
  FaceColorDefault = [0 0 0];
  EdgeColorDefault = [0 0 0];

  if (nbInputArguments == 3)
    % h = patch(X, Y, C) 
    XData = inputArguments{1};
    YData = inputArguments{2};
    CData = inputArguments{3};
    haveCData = true;
  elseif  (nbInputArguments == 4)
    % h = patch(X, Y, Z, C)
    XData = inputArguments{1};
    YData = inputArguments{2};
    ZData = inputArguments{3}; 
    CData = inputArguments{4}; 
    haveCData = true;
  elseif (nbInputArguments > 0) 
    error(_('Wrong number of input arguments.'));
  end

  
  if (isvector (XData))
    XData = XData(:);
    YData = YData(:);
    ZData = ZData(:);
    if (haveCData && isnumeric(CData))
      if (isvector (CData) && numel (CData) == numel (XData))
        CData = CData(:);
      elseif (size(CData, 1) ~= numel (XData) && size(CData, 2) == numel (XData))
        CData = CData .';
      end
    end
  end


  nbPropertiesList = length(propertiesList);
  ParentAsProperty = getValueByName(propertiesList, nbPropertiesList, 'Parent');
  XDataAsProperty = getValueByName(propertiesList, nbPropertiesList, 'XData');
  YDataAsProperty = getValueByName(propertiesList, nbPropertiesList, 'YData');
  ZDataAsProperty = getValueByName(propertiesList, nbPropertiesList, 'ZData');
  CDataAsProperty = getValueByName(propertiesList, nbPropertiesList, 'CData');
  FacesAsProperty = getValueByName(propertiesList, nbPropertiesList, 'Faces');
  VerticesAsProperty = getValueByName(propertiesList, nbPropertiesList, 'Vertices');
  FaceVertexCDataAsProperty = getValueByName(propertiesList, nbPropertiesList, 'FaceVertexCData');
  FaceColorAsProperty = getValueByName(propertiesList, nbPropertiesList, 'FaceColor');
  EdgeColorAsProperty = getValueByName(propertiesList, nbPropertiesList, 'EdgeColor');
  CDataMappingAsProperty = getValueByName(propertiesList, nbPropertiesList, 'CDataMapping');

  propertiesList = removeCollectedProperties(propertiesList, nbPropertiesList);

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

  if (haveCData && isvector(CData) && length(CData) ~= 3)
    colors = colormap(Parent);
    nbColors = size(colors, 1);
    if strcmp(CDataMapping, 'scaled')
      CLim = Parent.CLim;
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
      error(_('CDataMapping scaled or direct value expected'));
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
    if( size(XData) ~= size(YData) | size(XData) ~= size(ZData) | length(size(XData)) > 2 )
      error(_('Size of data mismatch.'));
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
      Faces = reshape(1:nx*ny, [ny, nx])';
    else
      Faces = reshape(1:nx*ny, size(XData))';
    end
  end

  if ~isempty(CDataAsProperty)
    CData = CDataAsProperty;
  else
    if ~haveCData
      CData = Parent.Color;
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
      error(_('String color or numeric value expected.')),
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
    if ischar(EdgeColor) && strcmp(EdgeColor, 'none')
      EdgeColor = CData;
    end
  else
      EdgeColor = EdgeColorDefault;
  end

  args = {'Parent', Parent, ...
  'XData', XData, ...
  'YData', YData, ...
  'ZData', ZData, ...
  'CData', CData, ...
  'Faces', Faces, ...
  'Vertices', Vertices, ...
  'FaceVertexCData', FaceVertexCData, ...
  'FaceColor', FaceColor, ...
  'EdgeColor', EdgeColor};

  args = [args, propertiesList];

end
%=============================================================================
function filteredProperties = removeCollectedProperties(propertiesList, nbPropertiesList)
  filteredProperties = propertiesList;
  elements = {'Parent', 'XData', 'YData', 'ZData', 'CData', 'Faces', ...
      'Vertices', 'FaceVertexCData', 'FaceColor', 'EdgeColor', 'CDataMapping'};
  nbElements = length(elements);
  for k = 1:1:nbElements
    idx = find(strcmp(filteredProperties, elements{k}));
    if ~isempty(idx)
      filteredProperties([idx, idx + 1]) = [];
    end
  end
end
%=============================================================================
function value = getValueByName(inputArguments, nbInputArguments, name)
  value = [];
  for k = 1:2:nbInputArguments
    fieldname = inputArguments{k};
    if ischar(fieldname) && strcmp(fieldname, name)
      value = inputArguments{k  + 1};
      return
    end
end
%=============================================================================
