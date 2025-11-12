%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = colorbar(varargin)
  % TO DO: (later) a dedicated graphics object.
  varargin = convertStringToCharArgs(varargin);
  nargoutchk(0, 1);
  width = 0.1; % relative thickness of colorbar (width for vertical, height for horizontal)
  inputArguments = varargin;
  % default location
  location = 'eastoutside';
  if length(inputArguments) > 0
    if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes')))
      ax = inputArguments{1};
      inputArguments = inputArguments(2:end);
    else
      ax = gca();
    end
  else
    ax = gca();
  end  
  % Parse a positional location string (e.g. colorbar(ax,'north')) or Name,Value 'Location'
  locationSpecified = false;
  orientationToken = '';
  offRequested = false;
  
  if ~isempty(inputArguments) && ischar(inputArguments{1})
    token = lower(inputArguments{1});
    if strcmp(token, 'off')
      offRequested = true;
      inputArguments = inputArguments(2:end);
    elseif any(strcmpi(token,{'north','south','east','west','northoutside','southoutside','eastoutside','westoutside','horizontal','vertical'}))
      % accept 'horizontal'/'vertical' as orientation-only shorthand; defer exact side
      if strcmp(token, 'horizontal')
        orientationToken = 'horizontal';
      elseif strcmp(token, 'vertical')
        orientationToken = 'vertical';
      else
        location = token;
      end
      inputArguments = inputArguments(2:end);
      locationSpecified = true;
    end
  end
  
  % Also accept a Name,Value pair 'Location', value
  if ~isempty(inputArguments)
    for k = 1:2:numel(inputArguments)-1
      if ischar(inputArguments{k}) && strcmpi(inputArguments{k}, 'Location')
        val = inputArguments{k+1};
        if ischar(val) && any(strcmpi(val,{ 'north','south','east','west','northoutside','southoutside','eastoutside','westoutside'}))
          location = lower(val);
          % remove the Location pair from the remaining set so set(cba,...) won't try to set it on the axes
          inputArguments(k:k+1) = [];
          locationSpecified = true;
        end
        break;
      end
    end
  end
  
  % Handle 'off' request early
  if offRequested
    cba = findColorBar(ax);
    cba = colorBarOff(ax, cba, width);
    if nargout == 1
      varargout{1} = cba;
    else
      varargout = {};
    end
    return;
  end
  
  % If user requested only an orientation ('horizontal'/'vertical'), pick the
  % best outside side based on available space around the axes.
  if ~isempty(orientationToken)
    origPosTmp = ax.OuterPosition;
    spaceBelow = origPosTmp(2);
    spaceAbove = 1 - (origPosTmp(2) + origPosTmp(4));
    spaceLeft = origPosTmp(1);
    spaceRight = 1 - (origPosTmp(1) + origPosTmp(3));
    if strcmp(orientationToken, 'horizontal')
      if spaceBelow >= spaceAbove
        location = 'southoutside';
      else
        location = 'northoutside';
      end
    elseif strcmp(orientationToken, 'vertical')
      if spaceRight >= spaceLeft
        location = 'eastoutside';
      else
        location = 'westoutside';
      end
    end
  end
  
  cba = findColorBar(ax);
  
  % Create or update colorbar
  if isempty(cba)
    % No existing colorbar, create one
    cba = createColorBar(ax, cba, width, location);
  else
    % Colorbar exists - if a location/orientation was specified, always recreate
    if locationSpecified || ~isempty(orientationToken)
      % Recreate at new position
      cba = colorBarOff(ax, cba, width);
      cba = createColorBar(ax, cba, width, location);
    end
  end
  
  % Apply any additional Name,Value properties
  if length(inputArguments) > 0
    set(cba, inputArguments{:});
  end
  
  if nargout == 1
    varargout{1} = cba;
  else
    varargout = {};
  end
end 
%=============================================================================
function cba = createColorBar(ax, dummy, width, location)
  % Save original axes outer position so we can restore it later
  origPos = ax.OuterPosition;
  
  % Clamp requested thickness to reasonable normalized bounds
  minCB = 0.02; % minimal normalized thickness
  maxCB = 0.5;  % maximal normalized thickness
  cb = max(min(width, maxCB), minCB);
  
  % Reserve a small gap between axes and colorbar
  gap = 0.005;
  
  % Normalize location
  if (nargin < 4 || isempty(location))
    location = 'eastoutside';
  end
  loc = lower(location);
  
  % Prepare new axes position and colorbar position npos
  newAxPos = origPos;
  npos = origPos;
  
  try
    if strcmp(loc, 'eastoutside')
      % shrink axes width and place colorbar to the right outside
      newAxPos(3) = origPos(3) - cb - gap;
      if (newAxPos(3) < 0.01)
        newAxPos(3) = max(origPos(3) * 0.6, 0.01);
        cb = origPos(3) - newAxPos(3) - gap;
        cb = max(cb, minCB);
      end
      npos = [newAxPos(1) + newAxPos(3) + gap, newAxPos(2), cb, newAxPos(4)];
      yorient = 'vertical';
    elseif strcmp(loc, 'southoutside')
      % shrink axes height and place colorbar below outside
      newAxPos(2) = origPos(2) + cb + gap;
      newAxPos(4) = origPos(4) - cb - gap;
      if (newAxPos(4) < 0.01)
        newAxPos(4) = max(origPos(4) * 0.6, 0.01);
        cb = origPos(4) - newAxPos(4) - gap;
        cb = max(cb, minCB);
        newAxPos(2) = origPos(2) + cb + gap;
      end
      npos = [newAxPos(1), origPos(2), newAxPos(3), cb];
      yorient = 'horizontal';
    elseif strcmp(loc, 'northoutside')
      % shrink axes height and place colorbar above outside
      newAxPos(4) = origPos(4) - cb - gap;
      if (newAxPos(4) < 0.01)
        newAxPos(4) = max(origPos(4) * 0.6, 0.01);
        cb = origPos(4) - newAxPos(4) - gap;
        cb = max(cb, minCB);
      end
      npos = [newAxPos(1), newAxPos(2) + newAxPos(4) + gap, newAxPos(3), cb];
      yorient = 'horizontal';
    elseif strcmp(loc, 'north')
      % shrink axes height and place colorbar at top inside with gap
      newAxPos(4) = origPos(4) - cb - 2*gap;
      if (newAxPos(4) < 0.01)
        newAxPos(4) = max(origPos(4) * 0.6, 0.01);
        cb = origPos(4) - newAxPos(4) - 2*gap;
        cb = max(cb, minCB);
      end
      npos = [newAxPos(1), newAxPos(2) + newAxPos(4) + gap, newAxPos(3), cb];
      yorient = 'horizontal';
    elseif strcmp(loc, 'south')
      % shrink axes height and place colorbar at bottom inside with gap
      newAxPos(2) = origPos(2) + cb + 2*gap;
      newAxPos(4) = origPos(4) - cb - 2*gap;
      if (newAxPos(4) < 0.01)
        newAxPos(4) = max(origPos(4) * 0.6, 0.01);
        cb = origPos(4) - newAxPos(4) - 2*gap;
        cb = max(cb, minCB);
        newAxPos(2) = origPos(2) + cb + 2*gap;
      end
      npos = [newAxPos(1), origPos(2) + gap, newAxPos(3), cb];
      yorient = 'horizontal';
    elseif strcmp(loc, 'westoutside')
      % shrink axes width and place colorbar to the left outside
      newAxPos(1) = origPos(1) + cb + gap;
      newAxPos(3) = origPos(3) - cb - gap;
      if (newAxPos(3) < 0.01)
        newAxPos(3) = max(origPos(3) * 0.6, 0.01);
        cb = origPos(3) - newAxPos(3) - gap;
        cb = max(cb, minCB);
        newAxPos(1) = origPos(1) + cb + gap;
      end
      npos = [origPos(1), newAxPos(2), cb, newAxPos(4)];
      yorient = 'vertical';
    elseif strcmp(loc, 'west')
      % shrink axes width and place colorbar at left inside with gap
      newAxPos(1) = origPos(1) + cb + 2*gap;
      newAxPos(3) = origPos(3) - cb - 2*gap;
      if (newAxPos(3) < 0.01)
        newAxPos(3) = max(origPos(3) * 0.6, 0.01);
        cb = origPos(3) - newAxPos(3) - 2*gap;
        cb = max(cb, minCB);
        newAxPos(1) = origPos(1) + cb + 2*gap;
      end
      npos = [origPos(1) + gap, newAxPos(2), cb, newAxPos(4)];
      yorient = 'vertical';
    elseif strcmp(loc, 'east')
      % shrink axes width and place colorbar at right inside with gap
      newAxPos(3) = origPos(3) - cb - 2*gap;
      if (newAxPos(3) < 0.01)
        newAxPos(3) = max(origPos(3) * 0.6, 0.01);
        cb = origPos(3) - newAxPos(3) - 2*gap;
        cb = max(cb, minCB);
      end
      npos = [newAxPos(1) + newAxPos(3) + gap, newAxPos(2), cb, newAxPos(4)];
      yorient = 'vertical';
    else
      % Fallback to eastoutside
      newAxPos(3) = origPos(3) - cb - gap;
      npos = [newAxPos(1) + newAxPos(3) + gap, newAxPos(2), cb, newAxPos(4)];
      yorient = 'vertical';
    end
  catch ex
    % on any error fallback to default right outside
    newAxPos = origPos;
    newAxPos(3) = origPos(3) - cb - gap;
    npos = [newAxPos(1) + newAxPos(3) + gap, newAxPos(2), cb, newAxPos(4)];
    yorient = 'vertical';
  end
  
  % Create colorbar axes and store the original axes position in UserData
  ud = struct('Axes', ax, 'PrevOuterPosition', origPos, 'Location', loc);
  
  % Configure axes properties based on orientation and location
  if strcmp(yorient, 'vertical')
    % Determine Y axis location based on position
    if any(strcmp(loc, {'westoutside', 'west'}))
      yAxisLoc = 'right';  % labels on right side for west colorbars
    else
      yAxisLoc = 'right';  % labels on right side for east colorbars (default)
    end
    cba = axes('OuterPosition', npos, ...
    'YAxisLocation', yAxisLoc, ...
    'XTick', [], ...
    'Tag', 'Colorbar', ...
    'UserData', ud, ...
    'Colormap', ax.Colormap);
  else
    % Horizontal orientation - determine X axis location
    if any(strcmp(loc, {'northoutside', 'north'}))
      xAxisLoc = 'top';  % labels on top for north colorbars
    else
      xAxisLoc = 'bottom';  % labels on bottom for south colorbars
    end
    cba = axes('OuterPosition', npos, ...
    'XAxisLocation', xAxisLoc, ...
    'YTick', [], ...
    'Tag', 'Colorbar', ...
    'UserData', ud, ...
    'Colormap', ax.Colormap);
  end
  
  % If we changed the axes size, apply it so the colorbar has room
  if ~isequal(newAxPos, origPos)
    ax.OuterPosition = newAxPos;
  end
  
  % orientation: rotate the image for horizontal colorbars
  cmap = colormap(ax);
  clen = size(cmap, 1);
  cext = ax.CLim;
  cdiff = (cext(2) - cext(1)) / (clen / 2);
  cmin = cext(1) + cdiff;
  cmax = cext(2) - cdiff;
  if strcmp(yorient, 'vertical')
    __image__('Parent', cba, 'XData', [0, 1], 'YData', [cmin, cmax], 'CData', [1 : clen]');
  else
    % For horizontal colorbar, repeat the row to make it visible
    __image__('Parent', cba, 'XData', [cmin, cmax], 'YData', [0, 1], 'CData', repmat([1 : clen], 10, 1));
  end
  axis('tight');
  axes(ax);
end 
%=============================================================================
function cba = colorBarOff(ax, cba, width)
  if ~isempty(cba)
    % Restore previous axes outer position if available (support legacy
    % UserData that was the axes handle and the new struct form)
    ud = get(cba, 'UserData');
    prevpos = [];
    if isstruct(ud) && isfield(ud, 'PrevOuterPosition')
      prevpos = ud.PrevOuterPosition;
    end
    delete(cba);
    if ~isempty(prevpos)
      ax.OuterPosition = prevpos;
    end
    cba = [];
  end
end
%=============================================================================
function cba = findColorBar(ax)
  fig = ancestor(ax, 'figure');
  peers = fig.Children;
  for i = 1:numel(peers)
    if ~isgraphics(peers(i), 'axes')
      continue;
    end
    if ~strcmp(get(peers(i), 'Tag'), 'Colorbar')
      continue;
    end
    % Support both legacy UserData (axes handle) and new struct form
    ud = get(peers(i), 'UserData');
    isMatch = false;
    if isequal(ud, ax)
      isMatch = true;
    elseif isstruct(ud) && isfield(ud, 'Axes') && isequal(ud.Axes, ax)
      isMatch = true;
    end
    if isMatch
      cba = peers(i);
      return;
    end
  end
  cba = [];
end
%=============================================================================
