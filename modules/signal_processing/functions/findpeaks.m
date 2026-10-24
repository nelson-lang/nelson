%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = findpeaks(varargin)
  % findpeaks - locate local maxima (peaks) in a 1-D signal
  %
  % Syntax:
  %   [pks, locs, widths, prominences] = findpeaks(Y)
  %   [pks, locs, widths, prominences] = findpeaks(Y, Fs, ...)
  %   [pks, locs, widths, prominences] = findpeaks(Y, X, ...)
  %
  % Inputs:
  %   Yin     - input signal (vector). Accepts row or column vectors.
  %   varargin- optional first positional arg is Fs (scalar) or X (vector),
  %              followed by name/value pairs:
  %              'MinPeakHeight', 'MinPeakProminence', 'Threshold',
  %              'MinPeakWidth', 'MaxPeakWidth', 'MinPeakDistance',
  %              'WidthReference' ('halfprom'|'halfheight'), 'SortStr',
  %              'NPeaks', 'Annotate'
  %
  % Outputs:
  %   pks         - peak amplitudes
  %   locs        - peak locations (X values, time or indices)
  %   widths      - widths at specified reference
  %   prominences - peak prominences
  %
  % Notes:
  %   - The algorithm proceeds in stages: candidate detection, height/threshold
  %     filtering, prominence computation, width computation, distance
  %     enforcement, sorting/limiting, and optional plotting.
  %   - The per-peak walks along the signal (prominence bases, width crossings
  %     and the minimum distance selection) are done by the __findpeaks__
  %     builtin, whose cost grows with the number of samples, not with the
  %     number of peaks times the walk length.

  narginchk(1, inf);
  nargoutchk(0, inf);
  Yin = varargin{1};
  validateattributes(Yin, {'numeric'}, {'real', 'vector', 'nonempty'}, mfilename(), 'Y');
  signal = Yin(:);
  numSamples = numel(signal);
  if numSamples < 3
    error('Nelson:signal:findpeaks:tooFewSamples', 'Input must have at least 3 samples.');
  end

  params = defaultParams();

  % pass remaining positional args (after Y) to the input parser
  otherArgs = varargin(2:end);
  [xVals, params] = parseInputs(otherArgs, numSamples, params);

  peakIndices = detectLocalMaxima(signal);

  peakIndices = applyHeightAndThreshold(signal, peakIndices, params);
  if isempty(peakIndices)
    [Ypk, Xpk, Wpk, Ppk] = emptyOutputs(Yin);
    returnOutputs(nargout, {Ypk, Xpk, Wpk, Ppk});
    return
  end

  [prominences, prominenceBaseY] = computeProminences(signal, peakIndices);

  keep = prominences >= params.MinPeakProminence;
  peakIndices = peakIndices(keep);
  prominences = prominences(keep);
  prominenceBaseY = prominenceBaseY(keep);
  if isempty(peakIndices)
    [Ypk, Xpk, Wpk, Ppk] = emptyOutputs(Yin);
    returnOutputs(nargout, {Ypk, Xpk, Wpk, Ppk});
    return
  end

  [widthX, widthY, peakWidths] = computeWidths(signal, xVals, peakIndices, prominences, params);

  [peakIndices, prominences, prominenceBaseY, widthX, widthY, peakWidths] = ...
    applyWidthFilter(peakIndices, prominences, prominenceBaseY, widthX, widthY, peakWidths, params);
  if isempty(peakIndices)
    [Ypk, Xpk, Wpk, Ppk] = emptyOutputs(Yin);
    returnOutputs(nargout, {Ypk, Xpk, Wpk, Ppk});
    return
  end

  [peakIndices, prominences, prominenceBaseY, widthX, peakWidths] = ...
    enforceMinPeakDistance(xVals, peakIndices, signal, prominences, prominenceBaseY, widthX, peakWidths, params.MinPeakDistance);

  [Ypk, Xpk, Wpk, Ppk, peakIndices, widthX, widthY] = ...
    prepareOutputs(signal, xVals, peakIndices, prominences, widthX, widthY, peakWidths, params, Yin);

  if nargout > 0
    outList = {Ypk, Xpk, Wpk, Ppk};
    for k = 1:min(nargout, numel(outList))
      varargout{k} = outList{k};
    end
  end

  if nargout == 0
    plotResults(xVals, signal, Xpk, Ypk, widthX, widthY, prominenceBaseY, peakIndices, params);
  end
end
%=============================================================================
function params = defaultParams()
  % defaultParams - return a struct of default parameter values
  % The parameters can be overridden via name/value pairs parsed in parseInputs.
  params.MinPeakHeight = -Inf;
  params.MinPeakProminence = 0;
  params.Threshold = 0;
  params.MinPeakWidth = 0;
  params.MaxPeakWidth = Inf;
  params.MinPeakDistance = 0;
  params.WidthReference = 'halfprom'; % 'halfheight' also possible
  params.SortStr = 'none';
  params.NPeaks = inf;
  params.Annotate = 'peaks';
end
%=============================================================================
function [xVals, params] = parseInputs(args, numSamples, params)
  % parseInputs - parse optional Fs/X positional arg and name/value pairs
  % Inputs:
  %   args       - varargin cell array from caller
  %   numSamples - length of the input signal (used to validate X)
  %   params     - struct of default params (will be updated)
  % Outputs:
  %   xVals      - x axis values (indices, time, or provided X)
  %   params     - updated params struct with any overrides
  xVals = (1:numSamples).';
  startArg = 1;
  if ~isempty(args) && ~ischar(args{1}) && ~isstring(args{1})
    first = args{1};
    if isscalar(first)
      Fs = first;
      xVals = ((0:numSamples - 1).') / Fs;
      startArg = 2;
    else
      validateattributes(first, {'numeric'}, {'vector', 'numel', numSamples}, mfilename(), 'X');
      xVals = first(:);
      startArg = 2;
    end
  end
  % parse name/value pairs (simple pairwise parser)
  if startArg <= numel(args)
    nv = args(startArg:end);
    if mod(numel(nv), 2) ~= 0
      error('Nelson:signal:findpeaks:nameValueNotPaired', 'Name-value arguments must be paired.');
    end
    for k = 1:2:numel(nv)
      name = nv{k};
      val = nv{k + 1};
      if ~ischar(name) && ~isstring(name)
        error('Nelson:signal:findpeaks:invalidParameterName', 'Parameter names must be strings.');
      end
      switch lower(char(name))
        case 'minpeakheight'
          params.MinPeakHeight = val;
        case 'minpeakprominence'
          params.MinPeakProminence = val;
        case 'threshold'
          params.Threshold = val;
        case 'minpeakwidth'
          params.MinPeakWidth = val;
        case 'maxpeakwidth'
          params.MaxPeakWidth = val;
        case 'minpeakdistance'
          params.MinPeakDistance = val;
        case 'widthreference'
          params.WidthReference = val;
        case 'sortstr'
          params.SortStr = val;
        case 'npeaks'
          params.NPeaks = val;
        case 'annotate'
          params.Annotate = val;
        otherwise
          error('Nelson:signal:unknownParameter', 'Unknown parameter name "%s".', name);
      end
    end
  end
  % validate param types/values (will raise descriptive errors if invalid)
  validateattributes(params.MinPeakHeight, {'numeric'}, {'scalar'}, mfilename(), 'MinPeakHeight');
  validateattributes(params.MinPeakProminence, {'numeric'}, {'scalar', 'nonnegative'}, mfilename(), 'MinPeakProminence');
  validateattributes(params.Threshold, {'numeric'}, {'scalar', 'nonnegative'}, mfilename(), 'Threshold');
  validateattributes(params.MinPeakWidth, {'numeric'}, {'scalar', 'nonnegative'}, mfilename(), 'MinPeakWidth');
  validateattributes(params.MaxPeakWidth, {'numeric'}, {'scalar', 'nonnegative'}, mfilename(), 'MaxPeakWidth');
  validateattributes(params.MinPeakDistance, {'numeric'}, {'scalar', 'nonnegative'}, mfilename(), 'MinPeakDistance');
  params.SortStr = validatestring(params.SortStr, {'none', 'ascend', 'descend'}, mfilename(), 'SortStr');
  params.WidthReference = validatestring(params.WidthReference, {'halfprom', 'halfheight'}, mfilename(), 'WidthReference');
end
%=============================================================================
function peakIndices = detectLocalMaxima(signal)
  % detectLocalMaxima - detect candidate peak indices (local maxima)
  % Strategy:
  %  - Treat positive Inf values specially (always peaks).
  %  - Replace +Inf entries with NaN in a temporary copy so they don't
  %    interfere with local-max detection, then add them back.
  %  - Bookend the signal with NaNs to handle edge samples uniformly.
  %  - Compress consecutive equal samples to avoid flat-region ambiguity,
  %    keeping the first sample of equal runs.
  %  - Use sign changes of the differenced compressed signal to find maxima.
  isPositiveInf = isinf(signal) & (signal > 0);
  signalTemp = signal;
  signalTemp(isPositiveInf) = NaN;
  signalBookend = [NaN; signalTemp; NaN];
  indexTemp = (1:numel(signalBookend)).';
  finiteMask = ~isnan(signalBookend);
  neqIdx = [1; 1 + find((signalBookend(1:end - 1) ~= signalBookend(2:end)) & (finiteMask(1:end - 1) | finiteMask(2:end)))];
  indexTemp = indexTemp(neqIdx);
  compressed = signalBookend(indexTemp);
  s = sign(compressed(2:end) - compressed(1:end - 1));
  iMax = 1 + find((s(2:end) - s(1:end - 1)) < 0);
  peakIndices = indexTemp(iMax) - 1;
  infIndices = find(isPositiveInf);
  peakIndices = union(peakIndices, infIndices);
end
%=============================================================================
function C = union(A, B)
  if isempty(A) && isempty(B)
    C = A([]); return
  end
  A = A(:); B = B(:);
  if ~(isnumeric(A) && isnumeric(B))
    error('Nelson:signal:unionNumericOnly', 'union implementation supports numeric inputs only.');
  end
  C = unique([A; B]);
end
%=============================================================================
function peakIndices = applyHeightAndThreshold(signal, peakIndices, params)
  if ~isempty(peakIndices)
    keep = signal(peakIndices) > params.MinPeakHeight;
    peakIndices = peakIndices(keep);
  end
  if ~isempty(peakIndices)
    neighborBase = max(signal(peakIndices - 1), signal(peakIndices + 1));
    keep = (signal(peakIndices) - neighborBase) >= params.Threshold;
    peakIndices = peakIndices(keep);
  end
end
%=============================================================================
function [prominences, prominenceBaseY] = computeProminences(signal, peakIndices)
  % computeProminences - compute the prominence of each peak
  % For each peak:
  %  - Walk left from the peak until a higher point is found (or the edge),
  %    the left base is the lowest sample met on the way.
  %  - Walk right similarly.
  %  - The prominence base is the higher of the two side minima.
  %  - Prominence = peak height - base height.
  % The walks are done by the __findpeaks__ builtin, which returns the
  % positions of the two side minima.
  [leftPos, rightPos] = __findpeaks__('bases', signal, peakIndices);
  leftBaseValY = double(signal(leftPos));
  rightBaseValY = double(signal(rightPos));
  prominenceBaseY = max(leftBaseValY, rightBaseValY);
  peakHeights = signal(peakIndices);
  if isinteger(peakHeights)
    peakHeights = double(peakHeights);
  end
  prominences = double(peakHeights - prominenceBaseY);
end
%=============================================================================
function [widthX, widthY, peakWidths] = computeWidths(signal, xVals, peakIndices, prominences, params)
  % computeWidths - compute left/right intercepts and widths for each peak
  % WidthReference:
  %  - 'halfprom': width measured at half the prominence above the base
  %  - 'halfheight': width measured at half the peak height (ignore negative peaks)
  % The __findpeaks__ builtin locates, on each side of a peak, the first
  % sample that is not above the reference level (a NaN sample stops the
  % search, the signal edge is reported when nothing qualifies); the intercept
  % is interpolated between that sample and its neighbour towards the peak.
  if isinteger(signal)
    signal = double(signal);
  end
  if isinteger(xVals)
    xVals = double(xVals);
  end
  peakHeights = signal(peakIndices);
  if strcmp(params.WidthReference, 'halfprom')
    refY = peakHeights - 0.5 * prominences;
  else % 'halfheight'
    refY = peakHeights / 2;
    refY(peakHeights < 0) = NaN;
  end
  skip = isnan(refY);
  [li, ri] = __findpeaks__('crossings', signal, peakIndices, refY);
  N = numel(signal);
  % left intercept (linear interpolation between neighbor samples)
  x1 = xVals(li); y1 = signal(li);
  ln = min(li + 1, N);
  x2 = xVals(ln); y2 = signal(ln);
  t = (refY - y1) ./ (y2 - y1);
  xl = double(x1 + t .* (x2 - x1));
  flat = (y2 == y1);
  xl(flat) = double(x1(flat));
  yl = double(refY);
  atPeak = (li == peakIndices);
  xl(atPeak) = double(xVals(li(atPeak)));
  yl(atPeak) = double(signal(li(atPeak)));
  % right intercept
  rp = max(ri - 1, 1);
  x1 = xVals(rp); y1 = signal(rp);
  x2 = xVals(ri); y2 = signal(ri);
  t = (refY - y1) ./ (y2 - y1);
  xr = double(x1 + t .* (x2 - x1));
  flat = (y2 == y1);
  xr(flat) = double(x2(flat));
  yr = double(refY);
  atPeak = (ri == peakIndices);
  xr(atPeak) = double(xVals(ri(atPeak)));
  yr(atPeak) = double(signal(ri(atPeak)));
  widthX = [xl xr];
  widthY = [yl yr];
  widthX(skip, :) = NaN;
  widthY(skip, :) = NaN;
  peakWidths = widthX(:, 2) - widthX(:, 1);
end
%=============================================================================
function [peakIndices, prominences, prominenceBaseY, widthX, widthY, peakWidths] = ...
  applyWidthFilter(peakIndices, prominences, prominenceBaseY, widthX, widthY, peakWidths, params)
  keep = (peakWidths >= params.MinPeakWidth) & (peakWidths <= params.MaxPeakWidth);
  keep = keep & ~isnan(peakWidths);
  peakIndices = peakIndices(keep);
  prominences = prominences(keep);
  prominenceBaseY = prominenceBaseY(keep);
  widthX = widthX(keep, :);
  widthY = widthY(keep, :);
  peakWidths = peakWidths(keep);
end
%=============================================================================
function [peakIndices_out, prominences_out, prominenceBaseY_out, widthX_out, peakWidths_out] = ...
  enforceMinPeakDistance(xVals, peakIndices, signal, prominences, prominenceBaseY, widthX, peakWidths, Pd)
  % enforceMinPeakDistance - remove peaks that are within Pd of a larger peak
  % Strategy: greedy algorithm that visits the peaks in descending amplitude
  % order and removes any smaller peak too close to an already-kept larger
  % peak. The visit is done by the __findpeaks__ builtin.
  if Pd <= 0
    peakIndices_out = peakIndices; prominences_out = prominences;
    prominenceBaseY_out = prominenceBaseY; widthX_out = widthX; peakWidths_out = peakWidths; return
  end
  peakLocs = xVals(peakIndices);
  peakAmplitudes = signal(peakIndices);
  [~, ord] = sort(peakAmplitudes, 'descend');
  if isinteger(peakLocs)
    peakLocs = double(peakLocs);
  end
  toKeep = __findpeaks__('distance', peakLocs, ord, double(Pd));
  peakIndices_out = peakIndices(toKeep);
  prominences_out = prominences(toKeep);
  prominenceBaseY_out = prominenceBaseY(toKeep);
  widthX_out = widthX(toKeep, :);
  peakWidths_out = peakWidths(toKeep);
end
%=============================================================================
function [Ypk, Xpk, Wpk, Ppk, peakIndices, widthX, widthY] = ...
  prepareOutputs(signal, xVals, peakIndices, prominences, widthX, widthY, peakWidths, params, Yin)
  peakAmplitudes = signal(peakIndices);
  peakLocs = xVals(peakIndices);
  % apply sort
  if ~strcmp(params.SortStr, 'none')
    [~, sidx] = sort(peakAmplitudes, params.SortStr);
    peakAmplitudes = peakAmplitudes(sidx); peakLocs = peakLocs(sidx);
    widthX = widthX(sidx, :); prominences = prominences(sidx); peakWidths = peakWidths(sidx); peakIndices = peakIndices(sidx);
  end
  % apply NPeaks
  Nmax = params.NPeaks;
  if isfinite(Nmax)
    Nkeep = min(numel(peakAmplitudes), round(Nmax));
    peakAmplitudes = peakAmplitudes(1:Nkeep); peakLocs = peakLocs(1:Nkeep);
    widthX = widthX(1:Nkeep, :); prominences = prominences(1:Nkeep); peakWidths = peakWidths(1:Nkeep); peakIndices = peakIndices(1:Nkeep);
  end
  % outputs
  Ypk = peakAmplitudes; Xpk = peakLocs; Wpk = peakWidths; Ppk = prominences;

  % Preserve input orientation: if Yin was a row vector, return outputs as rows
  if isrow(Yin)
    Ypk = Ypk.'; Xpk = Xpk.'; Wpk = Wpk.'; Ppk = Ppk.';
  end
end
%=============================================================================
function plotResults(xVals, signal, Xpk, Ypk, widthX, widthY, prominenceBaseY, peakIndices, params)
  hAxes = plotSignalWithPeaks(xVals, signal, peakIndices);
  if strcmp(params.Annotate, 'extents') && ~isempty(peakIndices)
    % map available data to the helper's expected arguments:
    %   bPk = prominenceBaseY
    %   bxPk = widthX (two columns: left/right X)
    %   byPk = widthY (two columns: left/right Y)
    %   wxPk = peak widths (compute if needed)
    wxPk = widthX(:, 2) - widthX(:, 1);
    plotExtents(hAxes, xVals, signal, peakIndices, prominenceBaseY, widthX, widthY, wxPk, params.WidthReference);
  end
  scalePlot(hAxes);
end
%=============================================================================
function hAxes = plotSignalWithPeaks(x, y, iPk)
  % plot signal
  hLine = plot(x, y, 'Tag', 'Signal');
  hAxes = ancestor(hLine, 'Axes');
  grid(hAxes, 'on');
  if numel(x) > 1
    try
      hAxes.XLim = hLine.XData([1 end]);
    catch
      % fallback for very small arrays
      hAxes.XLim = [min(x) max(x)];
    end
  end

  % use the color of the line for peak markers
  color = get(hLine, 'Color');
  hPk = line(hLine.XData(iPk), y(iPk), 'Parent', hAxes, ...
      'Marker', 'o', 'LineStyle', 'none', 'Color', color, 'Tag', 'Peak');

  plotpkmarkers(hPk, y(iPk));
end
%=============================================================================
function plotExtents(hAxes, x, y, iPk, bPk, bxPk, byPk, wxPk, refW)
  hold(hAxes, 'on');
  for k = 1:numel(iPk)
    idx = iPk(k);
    % plot width line (use provided widthX/widthY)
    if ~isempty(bxPk) && ~any(isnan(bxPk(k, :)))
      plot(hAxes, [bxPk(k, 1) bxPk(k, 2)], [byPk(k, 1) byPk(k, 2)], '-g', 'LineWidth', 1.2);
    end
    % plot prominence vertical line from base to peak
    if ~isempty(bPk) && ~isnan(bPk(k))
      plot(hAxes, [x(idx) x(idx)], [bPk(k) y(idx)], ':k');
    else
      plot(hAxes, [x(idx) x(idx)], [min(y) y(idx)], ':k');
    end
  end
  hold(hAxes, 'off');
end
%=============================================================================
function scalePlot(hAxes)
  % a simple autoscale with small padding
  xl = hAxes.XLim;
  yl = hAxes.YLim;
  % if limits are not set meaningfully, compute from children
  if isempty(xl) || any(isnan(xl))
    childX = get(hAxes, 'Children');
    try
      xdata = cell2mat(arrayfun(@(c) c.XData(:), childX, 'UniformOutput', false));
      xl = [min(xdata(:)) max(xdata(:))];
    catch
      xl = [0 1];
    end
  end
  if isempty(yl) || any(isnan(yl))
    childY = get(hAxes, 'Children');
    try
      ydata = cell2mat(arrayfun(@(c) c.YData(:), childY, 'UniformOutput', false));
      yl = [min(ydata(:)) max(ydata(:))];
    catch
      yl = [0 1];
    end
  end
  % add small padding
  xpad = diff(xl) * 0.02;
  if xpad == 0
    xpad = 1;
  end
  ypad = diff(yl) * 0.06;
  if ypad == 0
    ypad = 1;
  end
  hAxes.XLim = [xl(1) - xpad, xl(2) + xpad];
  hAxes.YLim = [yl(1) - ypad, yl(2) + ypad];
end
%=============================================================================
function plotpkmarkers(hLine, ypk)
  % Minimal styling for peak markers: inverted triangle feel
  try
    set(hLine, 'Marker', 'v', 'MarkerFaceColor', 'none', 'MarkerSize', 6);
  catch
    % ignore if properties can't be set
  end
end
%=============================================================================
function [Ypk, Xpk, Wpk, Ppk] = emptyOutputs(Yin)
  if isrow(Yin)
    Ypk = zeros(0, 1).'; Xpk = Ypk; Wpk = Ypk; Ppk = Ypk;
  else
    Ypk = zeros(0, 1); Xpk = Ypk; Wpk = Ypk; Ppk = Ypk;
  end
end
%=============================================================================
function returnOutputs(nout, outList)
  % Populate the caller's varargout with the requested outputs (used for early returns)
  if nout > 0
    nuse = min(nout, numel(outList));
    vo = cell(1, nuse);
    for k = 1:nuse
      vo{k} = outList{k};
    end
    assignin('caller', 'varargout', vo);
  end
end
%=============================================================================
function out = validatestring(str, choices, dummy, varname)
  if isstring(str)
    if isscalar(str)
      str = char(str);
    else
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', varname);
    end
  end
  if ~ischar(str)
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', varname);
  end

  s = lower(strtrim(str));
  matches = false(numel(choices), 1);
  for k = 1:numel(choices)
    matches(k) = strcmpi(s, lower(choices{k}));
  end

  if ~any(matches)
    error('Nelson:signal:invalidValue', 'Invalid value for %s. Expected one of: %s.', varname, strjoin(choices, ', '));
  end

  out = choices{find(matches, 1)};
end
%=============================================================================
function validateattributes(a, classes, attributes, dummy, varname)
  if ~iscell(classes)
    classes = {classes};
  end
  okClass = false;
  for k = 1:numel(classes)
    cls = classes{k};
    if (ischar(cls) || isstring(cls))
      if isa(a, char(cls))
        okClass = true; break
      end
    end
  end
  if ~okClass
    error('Nelson:signal:invalidClass', '%s must be one of class(es): %s.', varname, strjoin(cellfun(@char, classes, 'UniformOutput', false), ', '));
  end

  if isempty(attributes), return; end
  i = 1;
  while i <= numel(attributes)
    attr = attributes{i};
    if isstring(attr)
      attr = char(attr);
    end
    switch lower(attr)
      case 'real'
        if ~isreal(a)
          error('Nelson:validators:mustBeReal', '%s must be real.', varname);
        end
      case 'vector'
        if ~isvector(a)
          error('Nelson:validators:mustBeVector', '%s must be a vector.', varname);
        end
      case 'nonempty'
        if isempty(a)
          error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', varname);
        end
      case 'scalar'
        if ~isscalar(a)
          error('Nelson:validators:mustBeScalar', '%s must be a scalar.', varname);
        end
      case 'nonnegative'
        if ~isnumeric(a) || any(a(:) < 0)
          error('Nelson:validators:mustBeNonnegative', '%s must be nonnegative.', varname);
        end
      case 'numel'
        if i == numel(attributes)
          error('Nelson:signal:invalidUsage', 'Missing value for ''numel'' requirement.');
        end
        expected = attributes{i + 1};
        if (~isnumeric(expected) || (numel(a) ~= expected))
          error('Nelson:signal:sizeMismatch', '%s must have numel == %d.', varname, expected);
        end
        i = i + 1;
      otherwise
    end
    i = i + 1;
  end
end
%=============================================================================
