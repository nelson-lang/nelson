%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = scatter3(varargin)
  % scatter(x,y,z)
  % scatter(x,y,z, sz)
  % scatter(x,y,z, sz,c)
  % scatter(...,"filled")
  % scatter(...,mkr)
  % scatter(ax,...)
  % scatter(..., Name, Value)
  % s = scatter(...)
  
  narginchk(3, 100);
  nargoutchk(0, 1);
  
  [parent, X, Y, Z, scatterProperties] = parseArguments(convertStringToCharArgs(varargin));
  if isvector(X)
    if ~isfield(scatterProperties, 'CData')
      scatterProperties.CData = getColorAndUpdateIndex(parent);
    end
    scatterProperties = reshape([fieldnames(scatterProperties)'; struct2cell(scatterProperties)'], 1, []);
    h = __scatter__('Parent', parent, 'XData', X, 'YData', Y, 'ZData', Z, scatterProperties{:});
  else
    % X and Y are matrices
    if (size(X, 1) ~= size(Y, 1) && size(X, 1) ~= size(Z, 1))
      error(_('X, Y and Z must have the same number of rows.'));
    end
    if (size(X, 2) ~= size(Y, 2) && size(X, 2) ~= size(Z, 2))
      error(_('X, Y and Z must have the same number of columns.'));
    end
    h = [];
    
    for i = 1:size(X, 2)
      if isfield(scatterProperties, 'markerEdgeColor')
        markerColor = scatterProperties.markerEdgeColor;
      else
        markerColor = [];
      end
      if isfield(scatterProperties, 'CData')
        markerColor = scatterProperties.CData;
      end
      if isempty(markerColor)
        color = getColorAndUpdateIndex(parent);
      else
        color = markerColor;
      end
      scatterPropertiesBackup = scatterProperties;
      scatterProperties.CData = color; 
      
      scatterPropertiesC = reshape([fieldnames(scatterProperties)'; struct2cell(scatterProperties)'], 1, []);
      he = __scatter__('Parent', parent, 'XData', X(:, i), 'YData', Y(:, i), 'ZData', Z(:, i), scatterPropertiesC{:});
      scatterProperties = scatterPropertiesBackup;
      h = [h, he];
    end
  end
  
  if isscalar(h)
    H = h;
  else
    H = h(1);
  end
  ax = ancestor(H, 'axes');
  try
    if ~isempty(ax)
        switch ax.NextPlot
            case {'replaceall','replace'}
                view(ax, 3);
                grid(ax, 'on');
            case {'replacechildren'}
                view(ax, 3);
        end
    end
  end

  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, X, Y, Z, scatterProperties] = parseArguments(inputArguments)
  % Parse arguments for scatter function
  % Returns parent axes, X data, Y data, and scatter properties
  
  % Initialize default values
  scatterProperties = struct();
  
  % Check if first argument is an axes object
  if ~isempty(inputArguments) && isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'axes')
    parent = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    parent = [];
  end
  
  % Need at least X and Y data
  if length(inputArguments) < 2
    error(_('At least X and Y arguments are required.'));
  end
  
  % Extract X and Y data
  X = inputArguments{1};
  Y = inputArguments{2};
  Z = inputArguments{3};
  
  % Validate X and Y dimensions
  if (numel(X) ~= numel(Y))
    error(_('X and Y must be the same length.'));
  end
  
  % Remove X and Y from arguments
  inputArguments = inputArguments(4:end);
  
  % Create parent axes if not already set and no more arguments
  if isempty(inputArguments)
    if isempty(parent)
      parent = newplot();
    end
    return;
  end
  
  % Process Size data (SizeData)
  if ~isempty(inputArguments) && isnumeric(inputArguments{1})
    sizeArg = inputArguments{1};
    if isempty(sizeArg)
      % Empty size argument
      scatterProperties.SizeData = 36; % Default size
      inputArguments = inputArguments(2:end);
    elseif isscalar(sizeArg)
      % Single size for all points
      scatterProperties.SizeData = sizeArg;
      inputArguments = inputArguments(2:end);
    elseif numel(sizeArg) == numel(X)
      % Size vector matching data points
      scatterProperties.SizeData = sizeArg;
      inputArguments = inputArguments(2:end);
    else
      % Invalid size specification
      error(_('Size of S must match X, Y.'));
    end
  end
  
  
  % Extract special arguments: 'filled' and marker types
  [inputArguments, filledFlag] = extractSpecialArg(inputArguments, 'filled');
  if filledFlag
    scatterProperties.MarkerFaceColor = 'flat';
    scatterProperties.MarkerEdgeColor = 'none'; 
  end

  % Process Color data (CData)
  if ~isempty(inputArguments) && (isnumeric(inputArguments{1}) || isColorString(inputArguments{1}))
    colorArg = inputArguments{1};
    if ischar(colorArg) || isStringScalar(colorArg)
      colorArg = validatecolor(colorArg);
    end
    
    % Set marker edge color and CData
    scatterProperties.CData = colorArg;
    inputArguments = inputArguments(2:end);
  end
  
  
  % Extract marker symbol
  supportedMarkers = string(getMarkerNameList());
  [inputArguments, markerFound, markerValue] = extractMarker(inputArguments, supportedMarkers);
  if markerFound
    scatterProperties.Marker = markerValue;
  end
  
  % Process Name-Value pairs
  [propStruct, remainingArgs, dummy] = extractNameValuePairs(inputArguments{:});
  
  % Apply name-value properties (overriding any defaults)
  fields = fieldnames(propStruct);
  for i = 1:length(fields)
    scatterProperties.(fields{i}) = propStruct.(fields{i});
  end
 
  switch numel(remainingArgs)
    case 0
      % No more arguments, do nothing
      dummy;
    case 1
      % Only one argument left, check if it's a color or size
      arg = remainingArgs{1};
      if isnumeric(arg) || isColorString(arg)
        [colorValue, validColor, msg] = checkColorValue(arg, X, Y);
        if validColor
          scatterProperties.CData = colorValue;
        else
          error(msg);
        end
      else
        [sizeValue, validSize, msg] = checkSizeValue(arg, X, Y);
        if validSize
          scatterProperties.SizeData = sizeValue;
        else
          error(msg);
        end
      end
    otherwise
      error(_('Too many arguments.'));
    end 

  % Set default color if not specified
  if ~isfield(scatterProperties, 'CData') && ~isfield(scatterProperties, 'MarkerEdgeColor')
    if isempty(parent)
      parent = newplot();
    end
    scatterProperties.CData = getColorAndUpdateIndex(parent);
  end
  
  % Create parent axes if not already set
  if isempty(parent)
    parent = newplot();
  end
end
%=============================================================================
% Helper function to extract special arguments like 'filled'
function [remainingArgs, found, value] = extractSpecialArg(args, argName)
  found = false;
  value = [];
  idx = find(cellfun(@(x) (ischar(x) || isStringScalar(x)) && strcmpi(x, argName), args));
  if ~isempty(idx)
    found = true;
    args(idx) = [];  % Remove the special argument
  end
  
  remainingArgs = args;
end
%=============================================================================
% Helper function to extract marker
function [remainingArgs, found, value] = extractMarker(args, supportedMarkers)
  found = false;
  value = [];
  
  idxMarker = find(cellfun(@(x) (ischar(x) || isStringScalar(x)) && any(strcmpi(x, supportedMarkers)), args));
  if ~isempty(idxMarker)
    found = true;
    idx = idxMarker(end);
    value = args{idx};
    args(idx) = [];
    
    % Also remove 'Marker' property name if present
    if idx > 1
      previousArg = args{idx-1};
      if (ischar(previousArg) || isStringScalar(previousArg)) && strcmpi(previousArg, 'Marker')
        args(idx-1) = [];
      end
    end
  end
  
  remainingArgs = args;
end
%=============================================================================
% Helper function to check if a string is a valid color
function result = isColorString(str)
  supportedColorString = [string(getColorShortNameList()), string(getColorNameList())];
  result = (ischar(str) || isStringScalar(str)) && any(strcmpi(convertCharsToStrings(str), supportedColorString));
end
%=============================================================================
function [sizeValue, validSize, msg] = checkSizeValue(arg, X, Y)
  sizeValue = [];
  validSize = false;
  msg = '';

  if isnumeric(arg)
    if isempty(arg)
      sizeValue = []; % use default size
      validSize = true;
      return
    end

    if isscalar(arg)
      if arg > 0
        sizeValue = arg;
        validSize = true;
      else
        msg = _('Size must be a positive scalar.');
      end
      return
    end

    if isvector(arg) 
      if (size(X, 1) == size(arg, 1) || size(Y, 1) == size(arg, 1))
        sizeValue = arg;
        validSize = true;
        return
      end
    end

    if (ismatrix(X) || ismatrix(Y))
      if ismatrix(X) && size(arg, 1) == size(X, 1) && size(arg, 2) == size(X, 2)
        sizeValue = arg;
        validSize = true;
        return
      end

      if ismatrix(Y) && size(arg, 1) == size(Y, 1) && size(arg, 2) == size(Y, 2)
        sizeValue = arg;
        validSize = true;
        return
      end

    end

    msg = _('Size of S must match X, Y.');
  end
end
%=============================================================================
function [colorValue, validColor, msg] = checkColorValue(arg, X, Y)
  colorValue = [];
  validColor = false;
  msg = '';

  % Color names & short names are accepted as char or string
  if ischar(arg) || isstring(arg)
    try 
      colorValue = validatecolor(arg);
    catch
      msg = _('Invalid color name or short name.');
      return;
    end
    validColor = true;
    return;
  end

  % Must be numeric beyond this point
  if ~isnumeric(arg)
    msg = _('Color must be a string, character array, or numeric.');
    return;
  end

  % Single RGB triplet
  if isequal(size(arg), [1 3]) || isequal(size(arg), [3 1])
    if all(arg >= 0) && all(arg <= 1)
      colorValue = arg(:).'; % ensure row
      validColor = true;
      return;
    else
      msg = _('RGB triplet values must be between 0 and 1.');
      return;
    end
  end

  % Count number of points
  nPoints = numel(X);

  % Case: RGB triplet matrix for each point (X, Y must be vectors)
  if ismatrix(arg) && size(arg,2) == 3
    if isvector(X) && isvector(Y) && size(arg,1) == nPoints
      colorValue = arg;
      validColor = true;
      return;
    elseif ~isvector(X) || ~isvector(Y)
      % Matrix RGB colors per dataset
      if size(arg,1) == size(Y,2) % or size(X,2)
        colorValue = arg;
        validColor = true;
        return;
      else
        msg = _('RGB matrix rows must match number of points or datasets.');
        return;
      end
    else
      msg = _('RGB matrix rows must match number of points.');
      return;
    end
  end

  % Case: Colormap indices (only when x, y, sz are all vectors)
  if isvector(arg) && numel(arg) == nPoints
    if isvector(X) && isvector(Y)
      colorValue = arg(:); % ensure column
      validColor = true;
      return;
    else
      msg = _('Colormap index vector is only valid when X and Y are vectors.');
      return;
    end
  end

  msg = _('Invalid color format or size mismatch.');
end
%=============================================================================
