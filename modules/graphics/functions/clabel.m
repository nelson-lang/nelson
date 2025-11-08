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
  [contourMatrix, contourHandle, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing] = parse_inputs(varargin{:});

  % iterate contour matrix segments and place labels via helper
  matrixColPtr = 1;
  collectedHandles = [];
  nCols = size(contourMatrix,2);
  while (matrixColPtr < nCols)
    [segmentHandles, matrixColPtr] = place_labels_on_segment(contourMatrix, matrixColPtr, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing);
    if ~isempty(segmentHandles)
      % append without growing in a nested loop (still may reallocate, but fewer ops)
      collectedHandles(end+1:end+numel(segmentHandles)) = segmentHandles;
    end
  end
  if nargout > 0
    varargout{1} = collectedHandles;
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
function [contourMatrix, contourHandle, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing] = parse_inputs(varargin)
  % Return canonical contourMatrix, optional contourHandle (or empty),
  % levelsToLabel array, and remaining text property/value pairs.
  firstArg = varargin{1};
  textPropertyPairs = {};
  contourHandle = [];
  % default outputs
  uprightMode = false;
  labelSpacing = [];
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
      error(_('First argument to clabel must be the handle of a contour or a contour matrix C.'));
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
  for p = 1:2:numel(textPropertyPairs)
    if ischar(textPropertyPairs{p}) && strcmpi(textPropertyPairs{p}, 'Visible')
      hasVisibleProp = true;
      break;
    end
  end
  % detect LabelSpacing name-value (remove if present)
  for p = 1:2:numel(textPropertyPairs)
    if ischar(textPropertyPairs{p}) && strcmpi(textPropertyPairs{p}, 'LabelSpacing')
      if p+1 <= numel(textPropertyPairs)
        labelSpacing = textPropertyPairs{p+1};
        % remove these two entries
        textPropertyPairs(p:p+1) = [];
      end
      break;
    end
  end
  % upright mode: when no contour handle provided (C only)
  uprightMode = isempty(contourHandle);
  % If caller did not set Visible, we'll create text with Visible='off' and
  % toggle them on after all labels are created (faster rendering).
  forceToggleVisible = ~hasVisibleProp;
end
%=============================================================================
function levels = extract_levels_from_matrix(contourMatrix)
  % Walk the contour matrix headers and collect unique level values
  levelCandidates = [];
  colPtr = 1;
  while (colPtr < size(contourMatrix,2))
    levelCandidates(end+1) = contourMatrix(1,colPtr);
    numPoints = contourMatrix(2,colPtr);
    colPtr = colPtr + numPoints + 1;
  end
  levels = unique(levelCandidates);
end
%=============================================================================
function [segmentHandles, nextColPtr] = place_labels_on_segment(contourMatrix, colPtr, levelsToLabel, textPropertyPairs, forceToggleVisible, uprightMode, labelSpacing)
  % Process a single contour segment starting at column colPtr in contourMatrix.
  % Returns handles created for this segment and the pointer to the next segment.
  segmentHandles = [];
  lineStartIdx = colPtr + 1;
  lineStopIdx = contourMatrix(2,colPtr) + lineStartIdx - 1;
  segmentLevel = contourMatrix(1,colPtr);
  isClose = @(delta)abs(delta) < 100*eps;
  if (any(isClose(levelsToLabel - segmentLevel)))
  xPoints = contourMatrix(1,lineStartIdx:lineStopIdx);
  yPoints = contourMatrix(2,lineStartIdx:lineStopIdx);
    if numel(xPoints) < 2
      nextColPtr = lineStopIdx + 1;
      return;
    end
    deltaX = xPoints(2:end) - xPoints(1:end-1);
    deltaY = yPoints(2:end) - yPoints(1:end-1);
    segmentAngles = atan2(deltaY, deltaX) * 180 / pi;
    segmentLengths = sqrt(deltaX.^2 + deltaY.^2);
    cumulativeLengths = cumsum(segmentLengths);
    numSegmentPoints = numel(cumulativeLengths);
    if numSegmentPoints > 20
      startIndex = 10; % margin from the segment start
      if startIndex > numSegmentPoints
        startIndex = 1;
      end
      maxLen = cumulativeLengths(end);
      % use provided labelSpacing if specified (treated in same units as cumulativeLengths)
      if ~isempty(labelSpacing) && isnumeric(labelSpacing) && labelSpacing > 0
        threshold = labelSpacing;
      else
        threshold = maxLen / 3;
      end
      firstTarget = cumulativeLengths(startIndex);
      numLabels = floor((maxLen - firstTarget) / threshold) + 1;
      targets = firstTarget + (0:(numLabels-1)) * threshold;
      targetIdx = zeros(1, numel(targets));
      for k = 1:numel(targets)
        idxk = find(cumulativeLengths >= targets(k), 1);
        if isempty(idxk)
          targetIdx(k) = NaN;
        else
          targetIdx(k) = idxk;
        end
      end
      targetIdx = unique(targetIdx(~isnan(targetIdx)));
      % choose property pairs; if caller didn't set Visible, create as invisible
      if forceToggleVisible
        usedPairs = [textPropertyPairs, {'Visible', 'off'}];
      else
        usedPairs = textPropertyPairs;
      end
      % precompute label string once per segment
      labelStr = sprintf('%g', segmentLevel);
      nTargets = numel(targetIdx);
      if nTargets > 0
        % preallocate as cell array to safely hold graphics handles across
        tmpHandles = cell(1, nTargets * (1 + double(uprightMode))); % approx capacity
        cnt = 0;
        for k = 1:nTargets
          i = targetIdx(k);
          if uprightMode
            % upright text and a '+' marker (line object)
            cnt = cnt + 1;
            tmpHandles{cnt} = text(xPoints(i), yPoints(i), labelStr, 'Rotation', 0, usedPairs{:});
            % create a marker '+' as a line object
            cnt = cnt + 1;
            tmpHandles{cnt} = line(xPoints(i), yPoints(i), 'LineStyle', 'none', 'Marker', '+');
          else
            cnt = cnt + 1;
            tmpHandles{cnt} = text(xPoints(i), yPoints(i), labelStr, 'Rotation', segmentAngles(i), usedPairs{:});
          end
        end
        if cnt < numel(tmpHandles)
          tmpHandles = tmpHandles(1:cnt);
        end
        % convert cell to graphics object array
        if ~isempty(tmpHandles)
          segmentHandles = [tmpHandles{:}];
        end
      end
    else
      if forceToggleVisible
        usedPairs = [textPropertyPairs, {'Visible', 'off'}];
      else
        usedPairs = textPropertyPairs;
      end
      mid = max(1, floor(numSegmentPoints / 2));
      labelStr = sprintf('%g', segmentLevel);
      if uprightMode
        t = text(xPoints(mid), yPoints(mid), labelStr, 'Rotation', 0, usedPairs{:});
        l = line(xPoints(mid), yPoints(mid), 'LineStyle', 'none', 'Marker', '+');
        segmentHandles = [t, l];
      else
        segmentHandles = text(xPoints(mid), yPoints(mid), labelStr, ...
          'Rotation', segmentAngles(mid), usedPairs{:});
      end
    end
  end
  nextColPtr = lineStopIdx + 1;
end
%=============================================================================
