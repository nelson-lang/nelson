%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = legend(varargin)
  nargoutchk(0, 1);
  
  nbInputArguments = nargin;
  propertyIndex = 1;
  inputArguments = varargin;
  
  if nbInputArguments == 0
    ax = gca();
    cba = findLegend(ax);
    if (isempty(cba))
      children = ax.Children;
      for k = 1 : numel(children)
        inputArguments{k} = sprintf(_('data %d'), k);
      end
    else
      p = cba.UserData;
      inputArguments = p.args;
    end
  elseif nbInputArguments == 1
    if isgraphics(varargin{1})
      ax = varargin{1};
      propertyIndex = propertyIndex + 1;
    else
      ax = gca();
    end
  else
    if isgraphics(varargin{1})
      ax = varargin{1};
      propertyIndex = propertyIndex + 1;
    else
      ax = gca();
    end
  end
  if (nargin - propertyIndex == 0)
    option = convertStringsToChars(inputArguments{propertyIndex});
    if strcmpi(option, 'off')
      legendOff(ax);
      return;
    end
    if strcmpi(option, 'hide')
      legendHide(ax);
      return;
    end
    if strcmpi(option, 'show')
      legendShow(ax); 
      return;
    end
    if strcmpi(option, 'toggle')
      legendToggle(ax);
      return;
    end
    if strcmpi(option, 'boxon')
      legendBoxOn(ax);
      return; 
    end
    if strcmpi(option, 'boxoff')
      legendBoxOff(ax);
      return;
    end
  end
  
  if (nargin - propertyIndex > 0)
    if iscellstr(inputArguments{propertyIndex})
      args = inputArguments{propertyIndex}(:)';
      inputArguments = [args, inputArguments(propertyIndex + 1:end)];
    elseif isstring(inputArguments{propertyIndex})
      args = {};
      for s = inputArguments{propertyIndex}
        if ~ismissing(s)
          args = [args, s{1}];
        end
      end
      inputArguments = [args, inputArguments(propertyIndex + 1:end)];
    end
  end
  nbInputArguments = length(inputArguments);
  legendPositionOption = 'ne';
  if (nbInputArguments >= 2 && strcmpi(inputArguments{nbInputArguments - 1}, 'Location'))
    legendPositionOption = parseLocation(inputArguments{nbInputArguments});
    inputArguments((nbInputArguments - 1) : nbInputArguments) = [];
    nbInputArguments = nbInputArguments - 2;	
  end;
  for i = 1:nbInputArguments
    if (~ischar(inputArguments{i}))
      error(_('Invalid argument type: char vector, string or cell of char expected.')); 
    end;
  end 
  
  legendOff(ax);
  ax.Tag = ax.Tag;
  pos = ax.Position;
  parent = ax.Parent;
  parentPosition = parent.Position;
  figureSize = [parentPosition(3), parentPosition(4)];
  pos = pos .* [figureSize, figureSize];
  textHeight = ax.TextHeight;
  children = ax.Children;
  nbEntryCount = min(numel(ax.Children), nbInputArguments);
  padFactor = 1.1;
  legendRows = (nbEntryCount + 2) * textHeight * padFactor;
  maxlen = 0;
  for i=1:nbInputArguments
    maxlen = max(numel(inputArguments{i}), maxlen);
  end
  userData.handle = ax;
  userData.args = inputArguments;
  cba = axes('Visible', 'off', ...
  'OuterPosition', [0, 0, 0.01, 0.01], ...
  'XTick', [], ...
  'YTick', [], ...
  'XTickLabel', '', ...
  'YTickLabel', '', ...
  'ZTickLabel', '', ...
  'XLim', [0, 1], ...
  'YLim', [0, 1], ...
  'Tag', 'legend', ...
  'UserData', userData);
  textHandles = [];
  maxWidth = 0;
  maxHeight = 0;
  for i=1:nbEntryCount
    if isgraphics(children(i), 'line')
      textHandles(i) = __text__('Visible', 'off', ...
      'Position', [0, 0, 0], ...
      'FontName', ax.FontName, ...
      'FontSize', ax.FontSize, ...
      'FontAngle', ax.FontAngle, ...
      'FontWeight', ax.FontWeight, ...
      'String', inputArguments{i}, ...
      'VerticalAlignment', 'middle');
      
      boundingBox = textHandles(i).BoundingBox;
      maxWidth = max(maxWidth, boundingBox(3));
      maxHeight = max(maxHeight, boundingBox(4));
    end
  end;
  legendCols = maxWidth + 6 * textHeight;
  legendPosition = [0, 0, legendCols / figureSize(1), legendRows / figureSize(2)];
  nunx = textHeight / legendCols;
  nuny = textHeight / legendRows * padFactor;
  cba.OuterPosition = legendPosition;
  lineHandles = [];
  indexLineHandles = 0;
  for i = 1:nbEntryCount
    if isgraphics(children(i), 'line')
      ypos = 1 - (nuny + (i-1) * nuny + 0.5 * nuny);
      indexLineHandles = indexLineHandles + 1;
      lineHandles(indexLineHandles) = line('Visible', 'off', ...
      'XData', [1, 4] * nunx, ...
      'YData', [ypos,ypos], ...
      'Color', children(i).Color, ...
      'LineWidth', children(i).LineWidth, ... 
      'LineStyle', children(i).LineStyle);
      
      indexLineHandles = indexLineHandles + 1;
      lineHandles(indexLineHandles) = line('Visible', 'off', ...
      'XData', 2.5 * nunx, ...
      'YData', ypos, ...
      'Marker', children(i).Marker, ...
      'MarkerEdgeColor', children(i).MarkerEdgeColor, ...
      'MarkerFaceColor', children(i).MarkerFaceColor, ...
      'MarkerSize', children(i).MarkerSize);
      textHandles(i).Position = [5 * nunx, ypos, 0];
    end
  end
  [X, Y, W, H] = computePosition(legendPositionOption, legendPosition, ax.Position);
  cba.OuterPosition = [X, Y, W, H];
  cba.Visible = 'on';
  for k = 1:length(textHandles)
    textHandles(k).Visible = 'on';
  end
  for k = 1:length(lineHandles)
    lineHandles(k).Visible = 'on';
  end
  parentFigure = ancestor(ax, 'figure');
  parentFigure.CurrentAxes = ax;
  if nargout > 0
    varargout{1} = cba;
  end
end
%=============================================================================
function [X, Y, W, H] = computePosition(legendPositionOption, legendPosition, positionBox)
  cx = positionBox(1) + positionBox(3) / 2;
  ty = positionBox(2) + positionBox(4);
  by = positionBox(2);
  cy = positionBox(2) + positionBox(4) / 2;
  lx = positionBox(1);
  rx = positionBox(1) + positionBox(3);
  W = legendPosition(3);
  H = legendPosition(4);
  
  switch(legendPositionOption)
    case 'n'
      X = cx - W/2;
      Y = (ty - H) - (0.08 * H);
    case 's'
      X = cx - W/2;
      Y = (by) + (0.08 * H);
    case 'e'
      X = (rx - W) - (0.04 * W);
      Y = cy - H / 2;
    case 'w'
      X = (lx) + (0.04 * W);
      Y = cy - H/2;
    case 'ne'
      X = (rx - W) - (0.04 * W);
      Y = (ty - H) - (0.08 * H);
    case 'se'
      X = (rx - W) - (0.04 * W);
      Y = (by) + (0.08 * H);
    case 'sw'
      X = (lx) + (0.04 * W);
      Y = (by) + (0.08 * H);
    case 'nw'
      X = (lx) + (0.04 * W);
      Y = (ty - H) - (0.08 * H);
    otherwise
      error('Unrecognized position tag for legend');
    end
  end
  %=============================================================================
function locationOption = parseLocation(txt)
  isChar = ischar(txt) || isStringScalar(txt);
  if (~isChar)
    error(_('Unrecognized location option.'));
  end
  txt = convertStringsToChars(txt);
  txt = tolower(txt);
  switch(txt)
    case {'northeast', 'ne'}
      locationOption = 'ne';
    case {'southeast', 'se'}
      locationOption = 'se';
    case {'northwest', 'nw'}
      locationOption = 'nw';
    case {'southwest', 'sw'}
      locationOption = 'sw';
    case {'north', 'n'}
      locationOption = 'n';
    case {'east', 'e'}
      locationOption = 'e';
    case {'west', 'w'}
      locationOption = 'w';
    case {'south', 's'}
      locationOption = 's';
    otherwise
      error(_('Unrecognized location option.'));
    end
  end
  %=============================================================================
function cba = findLegend(ax)
  fig = ax.Parent;
  peers = fig.Children;
  for i = 1:numel(peers)
    if (isgraphics(peers(i), 'axes') && (strcmp(peers(i).Tag, 'legend')))
      userData = peers(i).UserData;
      if (isequal(userData.handle, ax))
        cba = peers(i);
        return;
      end
    end
  end
  cba = [];
end
%=============================================================================
function legendOff(ax)
  cba = findLegend(ax);
  if (~isempty(cba))
    fig = ax.Parent;
    peers = fig.Children;
    peers(peers == cba) = [];
    fig.Children = peers;
    refresh(fig);
  end
end
%=============================================================================
function recursiveShow(han)
  if (~isempty(han))
    han.Visible = 'on';
    children = han.Children;
    for i = children
      recursiveShow(i);
    end
  end
end
%=============================================================================
function recursiveHide(han)
  if (~isempty(han))
    han.Visible = 'off';
    children = han.Children;
    for i = children
      recursiveHide(i);
    end
  end
end
%=============================================================================
function legendShow(ax)
  cba = findLegend(ax);
  if (~isempty(cba))
    recursiveShow(cba);
  end
end
%=============================================================================
function legendHide(ax)
  cba = findLegend(ax);
  if (~isempty(cba))
    recursiveHide(cba);
  end
end
%=============================================================================
function legendToggle(ax)
  cba = findLegend(ax);
  if (~isempty(cba))
    if (strcmp(cba.Visible, 'on'))
      legendHide(ax);
    else
      legendShow(ax);
    end
  end
end
%=============================================================================
function legendBoxOff(ax)
  cba = findLegend(ax);
  if (~isempty(cba))
    cba.Visible = 'off';
  end
end
%=============================================================================
function legendBoxOn(ax)
  cba = findLegend(ax);
  if (~isempty(cba))
    cba.Visible = 'on';
  end
end
%=============================================================================
