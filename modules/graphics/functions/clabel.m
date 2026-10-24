%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = clabel(varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);

  % parse inputs into canonical form; detect if caller set Visible explicitly
  [contourMatrix, contourHandle, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing, returnEmptyPlaceholder, hasColorProp, hasHandleVisibilityProp] = parse_inputs(varargin{:});

  % Placement works in data coordinates. Querying axes/figure geometry here can
  % spuriously turn on the contour object's own native label rendering (which
  % would draw a second, duplicate set of labels), so we deliberately avoid it.
  scaleX = 1;
  scaleY = 1;

  % Default label spacing expressed in data units (empirically matched to the
  % expected label density). A user supplied LabelSpacing is given in points,
  % so it is mapped onto the same data-unit scale.
  if uprightMode
    defaultDataSpacing = 7.0;
  else
    defaultDataSpacing = 2.5;
  end
  if isempty(labelSpacing) || ~isnumeric(labelSpacing) || labelSpacing <= 0
    labelSpacing = defaultDataSpacing;
  else
    labelSpacing = labelSpacing * (defaultDataSpacing / 144);
  end

  % iterate contour matrix segments and place labels via helper
  matrixColPtr = 1;
  collectedHandles = [];
  nCols = size(contourMatrix, 2);
  while (matrixColPtr < nCols)
    [segmentHandles, matrixColPtr] = place_labels_on_segment(contourMatrix, matrixColPtr, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing, scaleX, scaleY, hasColorProp, hasHandleVisibilityProp);
    if ~isempty(segmentHandles)
      collectedHandles(end + 1:end + numel(segmentHandles)) = segmentHandles;
    end
  end
  if nargout > 0
    if returnEmptyPlaceholder
      varargout{1} = gobjects(0, 0);
    else
      varargout{1} = collectedHandles;
    end
  end
  % If we created text objects with visibility off to speed drawing, turn them on now
  if forceToggleVisible && ~isempty(collectedHandles)
    try
      set(collectedHandles, 'Visible', 'on');
    catch
      % ignore errors setting visibility (keep best-effort)
    end
  end
end
%=============================================================================
function [contourMatrix, contourHandle, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing, returnEmptyPlaceholder, hasColorProp, hasHandleVisibilityProp] = parse_inputs(varargin)
  % Return canonical contourMatrix, optional contourHandle (or empty),
  % levelsToLabel array, and remaining text property/value pairs.
  firstArg = varargin{1};
  textPropertyPairs = {};
  contourHandle = [];
  % default outputs
  uprightMode = false;
  labelSpacing = [];
  returnEmptyPlaceholder = false;
  % Support clabel([],h) where C is empty
  if (isempty(firstArg) && numel(varargin) > 1 && isgraphics(varargin{2}))
    contourHandle = varargin{2};
    contourMatrix = contourHandle.ContourMatrix;
    textPropertyPairs = varargin(3:end);
    levelsToLabel = contourHandle.LevelList;
  elseif (isnumeric(firstArg) || ismatrix(firstArg))
    contourMatrix = firstArg;
    if (numel(varargin) > 1)
      if isgraphics(varargin{2})
        contourHandle = varargin{2};
        textPropertyPairs = varargin(3:end);
        levelsToLabel = contourHandle.LevelList;
      else
        textPropertyPairs = varargin(2:end);
        levelsToLabel = extract_levels_from_matrix(contourMatrix);
      end
    else
      textPropertyPairs = {};
      levelsToLabel = extract_levels_from_matrix(contourMatrix);
    end
  else
    contourHandle = firstArg;
    if ~isgraphics(contourHandle)
      error('Nelson:clabel:invalidFirstArgumentTo', 'First argument to clabel must be the handle of a contour or a contour matrix C.');
    end
    if (numel(varargin) > 1)
      textPropertyPairs = varargin(2:end);
    end
    contourMatrix = contourHandle.ContourMatrix;
    levelsToLabel = contourHandle.LevelList;
  end
  % if the first remaining arg is not a char array, treat it as levels to label
  if (~isempty(textPropertyPairs) && (~ischar(textPropertyPairs{1})))
    levelsToLabel = textPropertyPairs{1};
    textPropertyPairs = textPropertyPairs(2:end);
  end
  % detect whether caller explicitly provided a 'Visible' property (do not override)
  hasVisibleProp = false;
  hasColorProp = false;
  hasHandleVisibilityProp = false;
  for p = 1:2:numel(textPropertyPairs)
    if ischar(textPropertyPairs{p}) && strcmpi(textPropertyPairs{p}, 'Visible')
      hasVisibleProp = true;
    end
    if ischar(textPropertyPairs{p}) && strcmpi(textPropertyPairs{p}, 'Color')
      hasColorProp = true;
    end
    if ischar(textPropertyPairs{p}) && strcmpi(textPropertyPairs{p}, 'HandleVisibility')
      hasHandleVisibilityProp = true;
    end
  end
  % detect LabelSpacing name-value (remove if present)
  for p = 1:2:numel(textPropertyPairs)
    if ischar(textPropertyPairs{p}) && strcmpi(textPropertyPairs{p}, 'LabelSpacing')
      if p + 1 <= numel(textPropertyPairs)
        labelSpacing = textPropertyPairs{p + 1};
        % remove these two entries
        textPropertyPairs(p:p + 1) = [];
      end
      break;
    end
  end
  % upright mode: when no contour handle provided (C only)
  uprightMode = isempty(contourHandle);
  returnEmptyPlaceholder = ~isempty(contourHandle);
  % If caller did not set Visible, we'll create text with Visible='off' and
  % toggle them on after all labels are created (faster rendering).
  forceToggleVisible = ~hasVisibleProp;
end
%=============================================================================
function levels = extract_levels_from_matrix(contourMatrix)
  % Walk the contour matrix headers and collect unique level values
  levelCandidates = [];
  colPtr = 1;
  while (colPtr < size(contourMatrix, 2))
    levelCandidates(end + 1) = contourMatrix(1, colPtr);
    numPoints = contourMatrix(2, colPtr);
    colPtr = colPtr + numPoints + 1;
  end
  levels = unique(levelCandidates);
end
%=============================================================================
function [segmentHandles, nextColPtr] = place_labels_on_segment(contourMatrix, colPtr, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing, scaleX, scaleY, hasColorProp, hasHandleVisibilityProp)
  % Process a single contour segment (one polyline for one level) starting at
  % column colPtr in contourMatrix. Returns the handles created for this
  % segment and the pointer to the next segment.
  segmentHandles = [];
  lineStartIdx = colPtr + 1;
  numPoints = contourMatrix(2, colPtr);
  lineStopIdx = numPoints + lineStartIdx - 1;
  segmentLevel = contourMatrix(1, colPtr);
  nextColPtr = lineStopIdx + 1;

  isClose = @(delta) abs(delta) < 100 * eps;
  if ~any(isClose(levelsToLabel - segmentLevel))
    return;
  end

  xPoints = contourMatrix(1, lineStartIdx:lineStopIdx);
  yPoints = contourMatrix(2, lineStartIdx:lineStopIdx);
  if numel(xPoints) < 2
    return;
  end

  % arc length of the polyline (scaleX/scaleY are 1, i.e. data units).
  deltaXpts = (xPoints(2:end) - xPoints(1:end - 1)) * scaleX;
  deltaYpts = (yPoints(2:end) - yPoints(1:end - 1)) * scaleY;
  segLenPts = sqrt(deltaXpts .^ 2 + deltaYpts .^ 2);
  cumLenPts = [0, cumsum(segLenPts)];
  totalLenPts = cumLenPts(end);
  if totalLenPts <= 0
    return;
  end

  % how many labels to place on this polyline: roughly one per `labelSpacing`
  % of arc length, but always at least one for a matching contour line.
  numLabels = max(1, round(totalLenPts / labelSpacing));

  % text/marker color defaults to black unless the caller supplied a color
  if hasColorProp
    colorPairs = {};
  else
    colorPairs = {'Color', [0 0 0]};
  end
  if uprightMode
    % keep the '+' marker at the exact contour point and let the number sit
    % just above/right of it, so the marker stays visible (expected placement).
    alignPairs = {'HorizontalAlignment', 'left', 'VerticalAlignment', 'bottom'};
  else
    alignPairs = {'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle'};
  end
  if forceToggleVisible
    visiblePairs = {'Visible', 'off'};
  else
    visiblePairs = {};
  end
  if hasHandleVisibilityProp || uprightMode
    handleVisibilityPairs = {};
  else
    handleVisibilityPairs = {'HandleVisibility', 'off'};
  end
  usedPairs = [textPropertyPairs, colorPairs, alignPairs, visiblePairs, handleVisibilityPairs];

  labelStr = sprintf('%g', segmentLevel);

  handleList = cell(1, numLabels * 2);
  cnt = 0;
  for k = 1:numLabels
    % distribute labels evenly with a half-step margin at both ends; a single
    % label lands at the midpoint of the polyline.
    target = (k - 0.5) * totalLenPts / numLabels;
    [px, py, angleDeg] = interp_point_and_angle(xPoints, yPoints, cumLenPts, deltaXpts, deltaYpts, segLenPts, target);
    if uprightMode
      markerHandle = line(px, py, 'LineStyle', 'none', 'Marker', '+', 'Color', [0 0 0], handleVisibilityPairs{:});
      textHandle = text(px, py, labelStr, 'Rotation', 0, usedPairs{:});
      cnt = cnt + 1;
      handleList{cnt} = textHandle;
      cnt = cnt + 1;
      handleList{cnt} = markerHandle;
    else
      cnt = cnt + 1;
      handleList{cnt} = text(px, py, labelStr, 'Rotation', angleDeg, usedPairs{:});
    end
  end
  if cnt > 0
    handleList = handleList(1:cnt);
    segmentHandles = [handleList{:}];
  end
end
%=============================================================================
function [px, py, angleDeg] = interp_point_and_angle(xPoints, yPoints, cumLenPts, deltaXpts, deltaYpts, segLenPts, target)
  % Locate the point at arc-length `target` along the
  % polyline, interpolating between vertices, and return the local tangent
  % angle (in degrees) normalized so the text is never upside down.
  nseg = numel(segLenPts);
  idx = find(cumLenPts(2:end) >= target, 1);
  if isempty(idx)
    idx = nseg;
  end
  segStart = cumLenPts(idx);
  segLen = segLenPts(idx);
  if segLen > 0
    frac = (target - segStart) / segLen;
  else
    frac = 0;
  end
  frac = min(max(frac, 0), 1);
  px = xPoints(idx) + frac * (xPoints(idx + 1) - xPoints(idx));
  py = yPoints(idx) + frac * (yPoints(idx + 1) - yPoints(idx));
  % local tangent of the contour line, used as the text rotation
  angleDeg = atan2(deltaYpts(idx), deltaXpts(idx)) * 180 / pi;
  % keep text within [-90, 90] degrees: flip by 180 when it would be upside down
  if angleDeg > 90
    angleDeg = angleDeg - 180;
  elseif angleDeg < -90
    angleDeg = angleDeg + 180;
  end
end
%=============================================================================
