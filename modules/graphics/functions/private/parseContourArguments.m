%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ax, contourProperties] = parseContourArguments(inputArguments, filled)
  inputArguments = convertStringToCharArgs(inputArguments);
  if isempty(inputArguments)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end

  ax = [];
  if (length(inputArguments{1}) == 1) && isgraphics(inputArguments{1}, 'axes')
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  end
  if isempty(inputArguments)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end

  [dataArguments, propertyArguments] = splitContourPropertyPairs(inputArguments);

  propertiesList = struct();
  if filled
    propertiesList.FaceColor = 'flat';
    propertiesList.EdgeColor = graphicsDefaultEdgeColor();
  else
    propertiesList.FaceColor = 'none';
  end

  if ~isempty(dataArguments) && ischar(dataArguments{end})
    [ps, cs, ms, msg] = colstyle(dataArguments{end}, 'plot', false);
    if isempty(msg)
      dataArguments(end) = [];
      propertiesList.LineStyle = ps;
      if isempty(cs)
        if filled
          propertiesList.EdgeColor = graphicsDefaultEdgeColor();
        else
          propertiesList.EdgeColor = 'flat';
        end
      else
        propertiesList.EdgeColor = cs;
      end
    end
  end

  [X, Y, Z, levels] = parseContourDataArguments(dataArguments);
  validateContourData(X, Y, Z);
  % Logical Z (e.g. a mask from a comparison) is contoured as 0/1.
  if islogical(Z)
    Z = double(Z);
  end
  warnContourNotRendered(Z);

  if ~isempty(X)
    propertiesList.XData = X;
  end
  if ~isempty(Y)
    propertiesList.YData = Y;
  end
  propertiesList.ZData = Z;
  if ~isempty(levels)
    if filled
      if numel(levels) > 1
        propertiesList.FillBelowLevel = 'off';
      else
        propertiesList.FillBelowLevel = 'on';
      end
    end
    levels = normalizeContourLevels(Z, levels);
    % A count of levels asked over data with no finite value normalizes to
    % nothing. Writing that empty list would flip LevelListMode to manual;
    % leaving the property alone keeps the automatic mode.
    if ~isempty(levels)
      propertiesList.LevelList = levels;
    end
  end

  contourProperties = reshape([fieldnames(propertiesList)'; struct2cell(propertiesList)'], 1, []);
  if ~isempty(propertyArguments)
    contourProperties = [contourProperties, canonicalizeContourPropertyPairs(propertyArguments)];
  end
end
%=============================================================================
function [X, Y, Z, levels] = parseContourDataArguments(dataArguments)
  X = [];
  Y = [];
  Z = [];
  levels = [];
  nbArguments = length(dataArguments);
  if nbArguments == 1
    Z = dataArguments{1};
  elseif nbArguments == 2
    Z = dataArguments{1};
    levels = dataArguments{2};
  elseif nbArguments == 3
    X = dataArguments{1};
    Y = dataArguments{2};
    Z = dataArguments{3};
  elseif nbArguments == 4
    X = dataArguments{1};
    Y = dataArguments{2};
    Z = dataArguments{3};
    levels = dataArguments{4};
  elseif nbArguments < 1
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  else
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
%=============================================================================
function [dataArguments, propertyArguments] = splitContourPropertyPairs(inputArguments)
  propertyStart = [];
  for k = 1:length(inputArguments)
    if ischar(inputArguments{k}) && isContourPropertyName(inputArguments{k})
      propertyStart = k;
      break
    end
  end
  if isempty(propertyStart)
    dataArguments = inputArguments;
    propertyArguments = {};
  else
    dataArguments = inputArguments(1:propertyStart - 1);
    propertyArguments = inputArguments(propertyStart:end);
    if mod(length(propertyArguments), 2) ~= 0
      error('Nelson:graphics:nameValuePair', 'Name-value arguments must be paired.');
    end
  end
end
%=============================================================================
function tf = isContourPropertyName(name)
  tf = ~isempty(canonicalContourPropertyName(name));
end
%=============================================================================
function propertyPairs = canonicalizeContourPropertyPairs(propertyPairs)
  for k = 1:2:length(propertyPairs)
    canonicalName = canonicalContourPropertyName(propertyPairs{k});
    if isempty(canonicalName)
      error('Nelson:graphics:invalidPropertyName', 'Invalid property name.');
    end
    propertyPairs{k} = canonicalName;
  end
end
%=============================================================================
function canonicalName = canonicalContourPropertyName(name)
  canonicalName = '';
  names = {'BeingDeleted', 'BusyAction', 'Children', 'ContourMatrix', 'CreateFcn', ...
    'DeleteFcn', 'DisplayName', 'EdgeAlpha', 'EdgeColor', 'FaceAlpha', ...
    'FaceColor', 'Fill', 'Floating', 'Interruptible', 'LabelColor', 'LabelFormat', ...
    'LabelSpacing', 'LevelList', 'LevelListMode', 'LevelStep', 'LevelStepMode', ...
    'LineColor', 'LineStyle', 'LineWidth', 'Parent', 'ShowText', 'Tag', 'TextList', ...
    'TextListMode', 'TextStep', 'TextStepMode', 'Type', 'UserData', 'Visible', ...
    'XData', 'XDataMode', 'YData', 'YDataMode', 'ZData', 'ZLocation'};
  for k = 1:length(names)
    if strcmpi(name, names{k})
      canonicalName = names{k};
      return
    end
  end
end
%=============================================================================
