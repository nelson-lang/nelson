%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = wordcloud(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [parent, args] = parseCloudParent(args);
  [words, sizes, args, defaultTitle, sourceTable, wordVariable, sizeVariable] = parseWordCloudData(args);
  opts = parseWordCloudOptions(args);
  if ~opts.TitleSpecified && ~isempty(defaultTitle)
    opts.Title = defaultTitle;
  end
  ax = cloudAxes(parent);
  chartParent = ax.Parent;
  if isgraphics(chartParent, 'figure') && isequal(chartParent.Color, groot().DefaultFigureColor)
    chartParent.Color = [1 1 1];
  end
  [displayWords, displaySizes] = displayedWords(words, sizes, opts.MaxDisplayWords);
  positionArgs = explicitPositionArgs(opts);
  opts = rmfield(opts, {'TitleSpecified', 'OuterPositionSpecified', 'InnerPositionSpecified', ...
    'PositionSpecified', 'UnitsSpecified', 'PositionConstraintSpecified'});
  h = __wordcloudchart__('AutoParent', 'off', 'Parent', chartParent, ...
    'WordData', words, 'SizeData', sizes, ...
    'MaxDisplayWords', opts.MaxDisplayWords, 'Color', opts.Color, ...
    'HighlightColor', opts.HighlightColor, 'Shape', opts.Shape, 'LayoutNum', opts.LayoutNum, ...
    'SizePower', opts.SizePower, 'FontName', opts.FontName, 'Box', opts.Box, 'Title', opts.Title, ...
    'TitleFontName', opts.TitleFontName, positionArgs{:}, 'SourceTable', sourceTable, ...
    'WordVariable', wordVariable, 'SizeVariable', sizeVariable);
  [textHandles, layout] = drawWords(ax, displayWords, displaySizes, opts);
  axis(ax, 'off');
  installWordCloudResizeFcn(chartParent, ax, textHandles, layout);
  h.UserData = struct('ChartType', 'wordcloud', 'Words', {words}, 'SizeData', sizes, 'Options', opts);
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseCloudParent(args)
  parent = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    error('Nelson:wordcloud:wordCloudChartCannotBeA', 'WordCloudChart cannot be a child of Axes.');
  end
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'figure')
    parent = args{1};
    args = args(2:end);
  end
end
%=============================================================================
function ax = cloudAxes(parent)
  if isempty(parent)
    ax = newplot();
  elseif isgraphics(parent, 'figure')
    ax = axes('Parent', parent);
  else
    error('Nelson:wordcloud:invalidParent', 'Parent must be a figure or axes.')
  end
  ax.Color = [1 1 1];
end
%=============================================================================
function [words, sizes, args, defaultTitle, sourceTable, wordVariable, sizeVariable] = parseWordCloudData(args)
  defaultTitle = '';
  sourceTable = [];
  wordVariable = '';
  sizeVariable = '';
  if isempty(args)
    error('Nelson:wordcloud:wordDataExpected', 'Word data expected.');
  end
  if (istable(args{1}) || istimetable(args{1})) && numel(args) >= 3
    sourceTable = args{1};
    [words, wordVariable] = graphicsTableVariable(sourceTable, args{2});
    [sizes, sizeVariable] = graphicsTableVariable(sourceTable, args{3});
    defaultTitle = sizeVariable;
    args = args(4:end);
  elseif numel(args) >= 2 && ~isWordCloudPropertyName(args{2})
    words = args{1};
    sizes = args{2};
    args = args(3:end);
  else
    [words, sizes] = categoryCounts(args{1});
    args = args(2:end);
  end
  words = graphicsLabelsFromValue(words, -1);
  if ~isnumeric(sizes)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Word sizes');
  end
  sizes = double(sizes(:)');
  if numel(words) ~= numel(sizes)
    error('Nelson:dimagree:wordsSizesNumberElements', 'Words and sizes must have the same number of elements.');
  end
end
%=============================================================================
function [words, counts] = categoryCounts(value)
  labels = graphicsLabelsFromValue(value, -1);
  words = {};
  counts = [];
  for k = 1:numel(labels)
    idx = find(strcmp(labels{k}, words), 1);
    if isempty(idx)
      words{end + 1, 1} = labels{k};
      counts(end + 1, 1) = 1;
    else
      counts(idx) = counts(idx) + 1;
    end
  end
end
%=============================================================================
function opts = parseWordCloudOptions(args)
  opts = struct('MaxDisplayWords', 100, 'Color', [0.380392156862745 0.380392156862745 0.380392156862745], ...
    'HighlightColor', [0.752941176470588 0.298039215686275 0.043137254901961], ...
    'Shape', 'oval', 'LayoutNum', 1, 'SizePower', 0.5, 'FontName', 'Helvetica', ...
    'Box', 'off', 'Title', '', 'TitleFontName', 'Helvetica', ...
    'OuterPosition', [0 0 1 1], 'InnerPosition', [0 0 1 0.9696], ...
    'Position', [0 0 1 0.9696], 'Units', 'normalized', ...
    'PositionConstraint', 'outerposition', 'TitleSpecified', false, ...
    'OuterPositionSpecified', false, 'InnerPositionSpecified', false, ...
    'PositionSpecified', false, 'UnitsSpecified', false, 'PositionConstraintSpecified', false);
  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(args)
    name = lower(char(args{k}));
    value = args{k + 1};
    switch name
      case 'maxdisplaywords'
        opts.MaxDisplayWords = floor(double(value));
        if ~isscalar(opts.MaxDisplayWords) || ~isfinite(opts.MaxDisplayWords) || opts.MaxDisplayWords <= 0
          error('Nelson:graphics:mustBePositiveScalar', '%s must be a positive scalar.', 'MaxDisplayWords');
        end
      case 'color'
        opts.Color = value;
      case 'highlightcolor'
        opts.HighlightColor = value;
      case 'shape'
        opts.Shape = lower(char(value));
      case 'layoutnum'
        opts.LayoutNum = floor(double(value));
      case 'sizepower'
        opts.SizePower = double(value);
      case 'fontname'
        opts.FontName = char(value);
      case 'titlefontname'
        opts.TitleFontName = char(value);
      case 'box'
        opts.Box = lower(char(value));
      case 'title'
        opts.Title = char(value);
        opts.TitleSpecified = true;
      case 'outerposition'
        opts.OuterPosition = double(value);
        opts.OuterPositionSpecified = true;
      case 'innerposition'
        opts.InnerPosition = double(value);
        opts.InnerPositionSpecified = true;
      case 'position'
        opts.Position = double(value);
        opts.PositionSpecified = true;
      case 'units'
        opts.Units = lower(char(value));
        opts.UnitsSpecified = true;
      case 'positionconstraint'
        opts.PositionConstraint = lower(char(value));
        opts.PositionConstraintSpecified = true;
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'wordcloud');
    end
  end
end
%=============================================================================
function args = explicitPositionArgs(opts)
  names = {'OuterPosition', 'InnerPosition', 'Position', 'Units', 'PositionConstraint'};
  flags = {'OuterPositionSpecified', 'InnerPositionSpecified', 'PositionSpecified', ...
    'UnitsSpecified', 'PositionConstraintSpecified'};
  args = {};
  for k = 1:numel(names)
    if opts.(flags{k})
      args = [args, {names{k}, opts.(names{k})}];
    end
  end
end
%=============================================================================
function tf = isWordCloudPropertyName(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  tf = any(strcmpi(char(value), {'MaxDisplayWords', 'Color', 'HighlightColor', 'Shape', ...
    'LayoutNum', 'SizePower', 'FontName', 'Box', 'Title', 'TitleFontName', ...
    'OuterPosition', 'InnerPosition', 'Position', 'Units', 'PositionConstraint'}));
end
%=============================================================================
function [words, sizes] = displayedWords(words, sizes, maxWords)
  [sizes, order] = sort(sizes(:), 'descend');
  words = words(order);
  n = min(numel(words), max(0, maxWords));
  words = words(1:n);
  sizes = sizes(1:n);
end
%=============================================================================
function [textHandles, layout] = drawWords(parent, words, sizes, opts)
  textHandles = [];
  layout = struct();
  if isempty(words)
    return
  end
  [x, y, fontSizes, useReferenceFrame, frame] = cloudLayout(parent, words, sizes, opts, 'initial');
  colors = wordColors(opts.Color, numel(words));
  nHighlighted = min(3 + strcmp(opts.Shape, 'rectangle'), numel(words));
  colors(1:nHighlighted, :) = repmat(validatecolor(opts.HighlightColor), nHighlighted, 1);
  for k = 1:numel(words)
    textHandles(end + 1, 1) = text(parent, x(k), y(k), words{k}, 'FontSize', fontSizes(k), ...
      'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
      'FontName', opts.FontName, 'Color', colors(k, :));
  end
  if useReferenceFrame
    parent.Position = [0 0 1 1];
    xlim(parent, frame.XLim);
    ylim(parent, frame.YLim);
    if ~isempty(opts.Title)
      textHandles(end + 1, 1) = text(parent, frame.TitleX, frame.TitleY, 0.5, opts.Title, 'FontSize', frame.TitleFontSize, 'FontWeight', 'bold', ...
        'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
        'FontName', opts.FontName, 'Color', [0 0 0]);
    end
  else
    pad = 0.35;
    xlim(parent, [min(x) - pad, max(x) + pad]);
    ylim(parent, [min(y) - pad, max(y) + pad]);
  end
  layout = wordCloudLayoutState(parent, textHandles, useReferenceFrame, words, sizes, opts);
end
%=============================================================================
function layout = wordCloudLayoutState(ax, textHandles, useReferenceFrame, words, sizes, opts)
  layout = struct();
  fig = ancestor(ax, 'figure');
  if isempty(fig) || ~isgraphics(fig) || isempty(textHandles)
    return
  end
  figPosition = fig.Position;
  if numel(figPosition) < 4 || figPosition(3) <= 0 || figPosition(4) <= 0
    return
  end
  if useReferenceFrame
    layout.FigureSize = figPosition(3:4);
    layout.Words = words;
    layout.Sizes = sizes;
    layout.Options = opts;
  else
    layout.FigureSize = figPosition(3:4);
  end
  layout.XLim = ax.XLim;
  layout.YLim = ax.YLim;
  layout.TextPositions = zeros(numel(textHandles), 3);
  layout.FontSizes = zeros(numel(textHandles), 1);
  for k = 1:numel(textHandles)
    layout.TextPositions(k, :) = textHandles(k).Position;
    layout.FontSizes(k) = textHandles(k).FontSize;
  end
end
%=============================================================================
function installWordCloudResizeFcn(parent, ax, textHandles, layout)
  if isempty(parent) || ~isgraphics(parent, 'figure') || isempty(textHandles) || isempty(fieldnames(layout))
    return
  end
  if ~isempty(parent.SizeChangedFcn)
    return
  end
  parent.SizeChangedFcn = @(src, event) resizeWordCloud(src, ax, textHandles, layout);
end
%=============================================================================
function resizeWordCloud(fig, ax, textHandles, layout)
  if isempty(fig) || ~isgraphics(fig) || isempty(ax) || ~isgraphics(ax)
    return
  end
  figPosition = fig.Position;
  if numel(figPosition) < 4 || any(layout.FigureSize <= 0)
    return
  end
  if isfield(layout, 'Words') && isfield(layout, 'Sizes') && isfield(layout, 'Options')
    [x, y, fontSizes, useReferenceFrame, frame] = cloudLayout(ax, layout.Words, ...
      layout.Sizes, layout.Options, 'resize');
    if useReferenceFrame
      ax.XLim = frame.XLim;
      ax.YLim = frame.YLim;
      nWords = numel(layout.Words);
      for k = 1:min(nWords, numel(textHandles))
        if isgraphics(textHandles(k))
          textHandles(k).Position = [x(k), y(k), 0];
          textHandles(k).FontSize = fontSizes(k);
        end
      end
      if numel(textHandles) > nWords && isgraphics(textHandles(nWords + 1))
        textHandles(nWords + 1).Position = [frame.TitleX, frame.TitleY, 0.5];
        textHandles(nWords + 1).FontSize = frame.TitleFontSize;
      end
      return
    end
  end
  scaleX = figPosition(3) / layout.FigureSize(1);
  scaleY = figPosition(4) / layout.FigureSize(2);
  ax.XLim = layout.XLim * scaleX;
  ax.YLim = layout.YLim * scaleY;
  for k = 1:numel(textHandles)
    if isgraphics(textHandles(k))
      pos = layout.TextPositions(k, :);
      pos(1) = pos(1) * scaleX;
      pos(2) = pos(2) * scaleY;
      textHandles(k).Position = pos;
      textHandles(k).FontSize = max(1, layout.FontSizes(k) * min(scaleX, scaleY));
    end
  end
end
%=============================================================================
function [x, y, fontSizes, useReferenceFrame, frame] = cloudLayout(parent, words, sizes, opts, layoutMode)
  n = numel(words);
  useReferenceFrame = false;
  frame = struct('XLim', [-1 1], 'YLim', [-1 1], 'TitleX', 0, 'TitleY', 0.95, 'TitleFontSize', 11);
  if n <= 8 && strcmp(opts.Shape, 'oval') && opts.LayoutNum == 1
    xTemplate = [1.10014997541214; 78.7692172533609; -25.7052404854014; -123.136124019849; ...
      -76.2725006964986; -53.6077445387689; 55.238349296669; 8.23005347930211];
    yTemplate = [8.36788729078359; -66.3826152561473; 105.898739466044; -50.9129471351014; ...
      -99.8343644766049; 66.4611486517226; 43.3446370993171; 55.1397396241978];
    rankSizes = [72; 60.3; 47.7; 37.8; 28.8; 22.5; 15.3; 7.2];
    x = xTemplate(1:n);
    y = yTemplate(1:n);
    fontSizes = rankSizes(1:n);
    if all(sizes == sizes(1))
      fontSizes = repmat(rankSizes(1), n, 1);
    end
    frame = struct('XLim', [-318.5 318.5], 'YLim', [-238.5 238.5], ...
      'TitleX', 0, 'TitleY', 232, 'TitleFontSize', 11);
    [fontSizes, frame] = fitFrameToAxes(parent, words, x, y, fontSizes, frame);
    useReferenceFrame = true;
    return
  end
  if n <= 8 && strcmp(opts.Shape, 'rectangle') && opts.LayoutNum == 1
    frame = rectangleReferenceFrame(parent, layoutMode);
    xTemplate = frame.X(:);
    yTemplate = frame.Y(:);
    rankSizes = frame.FontSize(:);
    x = xTemplate(1:n);
    y = yTemplate(1:n);
    fontSizes = rankSizes(1:n);
    if all(sizes == sizes(1))
      fontSizes = repmat(rankSizes(1), n, 1);
    end
    [fontSizes, frame] = fitFrameToAxes(parent, words, x, y, fontSizes, frame);
    useReferenceFrame = true;
    return
  end
  fontSizes = scaledSizes(sizes, 10, 100, opts.SizePower);
  [x, y] = cloudPositions(n, opts.Shape);
end
%=============================================================================
function [fontSizes, frame] = fitFrameToAxes(parent, words, x, y, fontSizes, frame)
  % Shrink the reference-frame font sizes so that the estimated data-space
  % extent of the words stays inside the layout frame for the actual figure
  % size. Font sizes are absolute (points) while the frame is expressed in
  % data units, so the largest words overflow the axes whenever the figure is
  % smaller than the size the frame template was captured at. The word extents
  % are estimated from the character count and the axes data-per-point ratio,
  % then all font sizes are scaled by a single factor (never enlarged) so the
  % cloud is bounded by the frame. This keeps the layout generic: it adapts to
  % the data (word lengths and counts) and to the axes/figure extent instead of
  % relying on a fixed shrink tuned for one example.
  if isempty(fontSizes)
    return
  end
  figSize = wordCloudFigureSize(parent);
  frameWidth = frame.XLim(2) - frame.XLim(1);
  frameHeight = frame.YLim(2) - frame.YLim(1);
  if figSize(1) <= 0 || figSize(2) <= 0 || frameWidth <= 0 || frameHeight <= 0
    return
  end
  pointsToPixels = 96 / 72;
  dataPerPointX = (frameWidth / figSize(1)) * pointsToPixels;
  dataPerPointY = (frameHeight / figSize(2)) * pointsToPixels;
  charWidthFactor = 0.5;
  marginX = 0.9;
  marginY = 0.88;
  nChars = cellfun(@(w) max(1, numel(char(w))), words(:));
  fontSizes = fontSizes(:);
  halfWidthUnit = 0.5 * charWidthFactor * nChars * dataPerPointX;
  halfHeightUnit = 0.5 * dataPerPointY;
  scale = 1;
  for iter = 1:8
    hw = scale * halfWidthUnit .* fontSizes;
    hh = scale * halfHeightUnit .* fontSizes;
    boxWidth = max(x(:) + hw) - min(x(:) - hw);
    boxHeight = max(y(:) + hh) - min(y(:) - hh);
    fitX = marginX * frameWidth / boxWidth;
    fitY = marginY * frameHeight / boxHeight;
    needed = min(fitX, fitY);
    if needed >= 1
      break
    end
    scale = scale * needed;
  end
  if scale < 1
    fontSizes = fontSizes * scale;
  end
  titleHalfHeight = 0.5 * frame.TitleFontSize * dataPerPointY;
  maxTitleY = frame.YLim(2) - 1.15 * titleHalfHeight;
  if frame.TitleY > maxTitleY
    frame.TitleY = maxTitleY;
  end
end
%=============================================================================
function frame = rectangleReferenceFrame(parent, layoutMode)
  figSize = wordCloudFigureSize(parent);
  if figSize(2) > figSize(1) * 1.45
    frame = rectangleTallFrame(layoutMode);
  elseif figSize(1) > figSize(2) * 2.05
    frame = rectangleWideFrame(layoutMode);
  elseif figSize(1) < 700 && figSize(2) < 500
    frame = rectangleSmallFrame();
  else
    frame = rectangleBaseFrame();
  end
end
%=============================================================================
function figSize = wordCloudFigureSize(parent)
  figSize = [1074 647];
  fig = ancestor(parent, 'figure');
  if isempty(fig) || ~isgraphics(fig)
    return
  end
  pos = fig.Position;
  if numel(pos) >= 4 && pos(3) > 0 && pos(4) > 0
    figSize = pos(3:4);
  end
end
%=============================================================================
function frame = rectangleBaseFrame()
  frame = makeRectangleFrame([-50; -321; -298; -245; 151; 414; -200; -352], ...
    [105; -97; -226; 250; 249; 135; 2; 108], ...
    [168; 152; 132; 125; 98; 80; 71; 55], [1070 622], 0.000510976140504, 313.187340153, 11);
end
%=============================================================================
function frame = rectangleSmallFrame()
  frame = makeRectangleFrame([-26.5; -166.5; -154.5; -96.5; -150.5; 205.5; 90.5; 194.5], ...
    [70; -64; -130; 8; 150; -66; 135; -150], ...
    [87; 79; 69; 65; 51; 42; 37; 29], [557 396], 0.000266357149428, 200.182364729, 11);
end
%=============================================================================
function frame = rectangleWideFrame(layoutMode)
  if strcmp(layoutMode, 'resize')
    frame = makeRectangleFrame([-46.5; -299.5; 145.5; 218.5; 330.5; -521.5; -444.5; 495.5], ...
      [44; -83; -98; 151; 48; 102; 191; 116], ...
      [117; 106; 92; 87; 68; 56; 49; 38], [1335 450], 0.000637596130446, 227.918632075, 11);
  else
    frame = makeRectangleFrame([-47.5; -301.5; 145.5; 218.5; 331.5; -522.5; -444.5; 553.5], ...
      [45; -83; -98; 153; 53; 104; -200; 161], ...
      [117; 106; 92; 87; 68; 56; 49; 38], [1335 450], 0.000637596130446, 227.918632075, 11);
  end
end
%=============================================================================
function frame = rectangleTallFrame(layoutMode)
  if strcmp(layoutMode, 'resize')
    frame = makeRectangleFrame([-7.5; -60.5; -99.5; 3.5; -106.5; 10.5; -36.5; 86.5], ...
      [168; -153; -311; 407; -408; 319; 262; 236], ...
      [100; 91; 79; 74; 58; 48; 42; 33], [481 908], 0.000230378044961, 456.941107185, 11);
  else
    frame = makeRectangleFrame([-7.5; -61.5; -100.5; 18.5; -120.5; 10.5; -117.5; 165.5], ...
      [168; -153; -311; 41; -408; -71; 363; 89], ...
      [100; 91; 79; 74; 58; 48; 42; 33], [481 908], 0.000230378044961, 456.941107185, 11);
  end
end
%=============================================================================
function frame = makeRectangleFrame(x, y, fontSize, layoutSize, titleX, titleY, titleFontSize)
  frame = struct();
  frame.X = x;
  frame.Y = y;
  frame.FontSize = fontSize;
  frame.XLim = [-layoutSize(1) / 2, layoutSize(1) / 2];
  frame.YLim = [-layoutSize(2) / 2, layoutSize(2) / 2];
  frame.TitleX = titleX;
  frame.TitleY = titleY;
  frame.TitleFontSize = titleFontSize;
end
%=============================================================================
function values = scaledSizes(sizes, lo, hi, sizePower)
  sizes = double(sizes(:));
  if max(sizes) == min(sizes)
    values = repmat((lo + hi) / 2, size(sizes));
  else
    values = lo + ((sizes - min(sizes)) ./ (max(sizes) - min(sizes))) .^ sizePower * (hi - lo);
  end
end
%=============================================================================
function [x, y] = cloudPositions(n, shape)
  if n <= 8
    xTemplate = [-0.12; 0.20; -0.25; -0.48; -0.31; -0.22; 0.08; -0.07];
    yTemplate = [-0.03; -0.29; 0.41; -0.25; -0.47; 0.23; 0.14; 0.16];
    x = xTemplate(1:n);
    y = yTemplate(1:n);
    return
  end
  angle = (0:n - 1)' * pi * (3 - sqrt(5));
  radius = sqrt((0:n - 1)' / max(1, n - 1));
  if strcmp(shape, 'rectangle')
    x = 2 * radius .* cos(angle);
    y = radius .* sin(angle);
  else
    x = 1.5 * radius .* cos(angle);
    y = radius .* sin(angle);
  end
end
%=============================================================================
function colors = wordColors(color, n)
  if isnumeric(color) && size(color, 2) == 3 && size(color, 1) == n
    colors = color;
  else
    baseColor = validatecolor(color);
    colors = repmat(baseColor, n, 1);
    for k = 4:n
      mix = (k - 1) / 198;
      colors(k, :) = min(max(baseColor + mix * [1 0.106 -0.482], 0), 1);
    end
  end
end
%=============================================================================
