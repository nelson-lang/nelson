%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = stackedplot(varargin)
  narginchk(1, Inf);
  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = extractStackedplotParent(inputArguments);
  [xValues, names, values, legendLabels, xLabelName, xVariableName, displayVariables, ...
    sourceTable, options] = ...
    stackedplotArguments(inputArguments);
  options.Parent = parent;

  if isempty(values)
    error('Nelson:stackedplot:noPlottableVariablesFound', 'No plottable variables found.');
  end
  if length(values) > 25
    error('Nelson:stackedplot:tooManyVariablesTo', 'Too many variables to plot.');
  end

  if ~isempty(options.DisplayLabels)
    names = normalizeLabels(options.DisplayLabels, length(values));
  end
  if ~isempty(options.XLabel)
    xLabelName = options.XLabel;
  end

  [fig, previousDrawLater] = beginStackedplotDrawLater(parent);
  try
    s = createStackedLineChart(parent);
    [axesHandles, lineProperties, axesProperties] = ...
      renderStackedplotAxes(xValues, names, values, legendLabels, xLabelName, options);
    set(s, ...
      'Children', axesHandles, ...
      'DisplayLabels', names(:), ...
      'DisplayVariables', displayVariables, ...
      'EventsVisible', options.EventsVisible, ...
      'CombineMatchingNames', options.CombineMatchingNames, ...
      'XLabel', xLabelName, ...
      'XVariable', xVariableName, ...
      'XData', options.XData, ...
      'YData', options.YData, ...
      'XLimits', options.XLimits, ...
      'SourceTable', sourceTable, ...
      'Title', options.Title, ...
      'LegendLabels', options.LegendLabels, ...
      'LegendVisible', options.LegendVisible, ...
      'LegendOrientation', options.LegendOrientation);
    applyStackedplotChartProperties(s, options);
    if strcmp(options.GridVisible, 'on')
      s.GridVisible = 'on';
    end
    set(s, ...
      'LineProperties', optionOrDefault(options.LineProperties, lineProperties), ...
      'AxesProperties', optionOrDefault(options.AxesProperties, axesProperties));
    if strcmp(options.LegendVisible, 'on') || ~isempty(options.AxesProperties)
      applyInitialAxesProperties(s);
    end
    removeStackedplotAxesFromFigure(fig, axesHandles);
    endStackedplotDrawLater(fig, previousDrawLater);
  catch exception
    endStackedplotDrawLater(fig, previousDrawLater);
    error(exception.message);
  end
end
%=============================================================================
function removeStackedplotAxesFromFigure(fig, axesHandles)
  if isempty(fig) || ~isgraphics(fig, 'figure')
    return
  end
  children = get(fig, 'Children');
  keptChildren = [];
  for k = 1:numel(children)
    keepChild = true;
    for j = 1:numel(axesHandles)
      if isequal(children(k), axesHandles(j))
        keepChild = false;
        break
      end
    end
    if keepChild
      keptChildren = [keptChildren children(k)];
    end
  end
  set(fig, 'Children', keptChildren);
end
%=============================================================================
function [parent, inputArguments] = extractStackedplotParent(inputArguments)
  parent = [];
  if ~isempty(inputArguments) && isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'figure')
    parent = inputArguments{1};
    inputArguments(1) = [];
  end
  if isempty(inputArguments)
    error('Nelson:stackedplot:tableTimetableOrExpected', 'table, timetable, or plottable array expected.');
  end
end
%=============================================================================
function s = createStackedLineChart(parent)
  s = __stackedplot__('AutoParent', 'off');
  if ~isempty(parent)
    s.Parent = parent;
  end
end
%=============================================================================
function [fig, previousDrawLater] = beginStackedplotDrawLater(parent)
  if isempty(parent)
    fig = gcf();
  else
    fig = parent;
  end
  previousDrawLater = fig.DrawLater;
  if strcmp(previousDrawLater, 'off')
    fig.DrawLater = 'on';
  end
end
%=============================================================================
function endStackedplotDrawLater(fig, previousDrawLater)
  if strcmp(previousDrawLater, 'off')
    fig.DrawLater = 'off';
  end
end
%=============================================================================
function [xValues, names, values, legendLabels, xLabelName, xVariableName, ...
    displayVariables, sourceTable, options] = stackedplotArguments(inputArguments)
  options = stackedplotDefaultOptions();
  xVariableName = '';
  displaySelector = [];
  sourceTable = [];

  if isTableInput(inputArguments{1})
    [tables, firstNameValue, sourceTable] = collectTableInputs(inputArguments);
    variableNames = combinedVariableNames(tables);
    if length(inputArguments) >= firstNameValue && ~isNameValueStart(inputArguments, firstNameValue)
      arg = inputArguments{firstNameValue};
      if isVariableSelector(arg, variableNames)
        displaySelector = arg;
        firstNameValue = firstNameValue + 1;
      elseif isLineSpecArgument(arg)
        options = applyLineSpec(options, arg);
        firstNameValue = firstNameValue + 1;
      else
        displaySelector = arg;
        firstNameValue = firstNameValue + 1;
      end
    end
    if length(inputArguments) >= firstNameValue && isLineSpecArgument(inputArguments{firstNameValue})
      options = applyLineSpec(options, inputArguments{firstNameValue});
      firstNameValue = firstNameValue + 1;
    end
    options = parseStackedplotOptions(inputArguments, firstNameValue, options);
    if ~isempty(options.DisplayVariables)
      displaySelector = options.DisplayVariables;
    end
    options.EventData = stackedplotEventData(tables);
    xVariableName = options.XVariable;
    [xValues, names, values, legendLabels, xLabelName, displayVariables] = ...
      tablesStackedplotData(tables, displaySelector, xVariableName, options.CombineMatchingNames);
  elseif isStackedPlotValue(inputArguments{1})
    firstNameValue = 2;
    y = inputArguments{1};
    x = [];
    if length(inputArguments) >= 2 && isStackedPlotValue(inputArguments{2})
      x = inputArguments{1};
      y = inputArguments{2};
      firstNameValue = 3;
    end
    if length(inputArguments) >= firstNameValue && isLineSpecArgument(inputArguments{firstNameValue})
      options = applyLineSpec(options, inputArguments{firstNameValue});
      firstNameValue = firstNameValue + 1;
    end
    options = parseStackedplotOptions(inputArguments, firstNameValue, options);
    [xValues, names, values, legendLabels, xLabelName, displayVariables] = ...
      matrixStackedplotData(x, y);
    if isempty(x)
      % The chart XData holds the auto sample index as a row vector, matching
      % the reference (the per-panel line children keep their own data).
      options.XData = (1:size(stackedplotValueToNumeric(y), 1));
    else
      options.XData = stackedplotValueToNumeric(x);
    end
    options.YData = stackedplotValueToNumeric(y);
  else
    error('Nelson:stackedplot:tableTimetableOrExpected', 'table, timetable, or plottable array expected.');
  end
end
%=============================================================================
function tf = isTableInput(value)
  tf = istable(value) || istimetable(value) || isCellArrayOfTables(value);
end
%=============================================================================
function tf = isCellArrayOfTables(value)
  tf = false;
  if ~iscell(value) || isempty(value)
    return
  end
  for k = 1:numel(value)
    if ~(istable(value{k}) || istimetable(value{k}))
      return
    end
  end
  tf = true;
end
%=============================================================================
function [tables, nextIndex, sourceTable] = collectTableInputs(inputArguments)
  if isCellArrayOfTables(inputArguments{1})
    tables = inputArguments{1}(:)';
    nextIndex = 2;
    sourceTable = inputArguments{1};
  else
    tables = {};
    nextIndex = 1;
    while nextIndex <= length(inputArguments) && ...
        (istable(inputArguments{nextIndex}) || istimetable(inputArguments{nextIndex}))
      tables{end + 1} = inputArguments{nextIndex};
      nextIndex = nextIndex + 1;
    end
    sourceTable = tables{1};
    if length(tables) > 1
      sourceTable = tables;
    end
  end
  validateTableInputTypes(tables);
end
%=============================================================================
function validateTableInputTypes(tables)
  firstIsTimetable = istimetable(tables{1});
  for k = 2:length(tables)
    if istimetable(tables{k}) ~= firstIsTimetable
      error('Nelson:stackedplot:invalidInputs', 'Inputs must be all tables or all timetables.');
    end
  end
end
%=============================================================================
function variableNames = combinedVariableNames(tables)
  variableNames = {};
  for k = 1:length(tables)
    names = tables{k}.Properties.VariableNames;
    for n = 1:length(names)
      if ~any(strcmp(variableNames, names{n}))
        variableNames{end + 1} = names{n};
      end
    end
  end
end
%=============================================================================
function tf = isVariableSelector(value, variableNames)
  tf = isnumeric(value) || islogical(value) || iscellstr(value);
  if tf
    return
  end
  if ischar(value)
    tf = any(strcmp(value, variableNames));
  end
end
%=============================================================================
function [xValues, names, values, legendLabels, xLabelName, displayVariables] = ...
    tablesStackedplotData(tables, displaySelector, xVariableName, combineMatchingNames)
  variableNames = combinedVariableNames(tables);
  explicitSelector = ~isempty(displaySelector);
  validateXVariableForTables(tables, xVariableName);
  xLabelName = defaultTableXLabel(tables{1}, xVariableName);
  displayVariables = {};
  names = {};
  values = {};
  xValues = {};
  legendLabels = {};

  if isGroupedVariableSelector(displaySelector)
    [xValues, names, values, legendLabels, displayVariables] = ...
      groupedTableSeries(tables, displaySelector, xVariableName);
  else
    candidateNames = resolveVariableSelector(displaySelector, variableNames);
    if ~combineMatchingNames
      [xValues, names, values, legendLabels, displayVariables] = ...
        separateTableSeries(tables, candidateNames, xVariableName, explicitSelector);
    else
      [xValues, names, values, legendLabels, displayVariables] = ...
        combinedTableSeries(tables, candidateNames, xVariableName, explicitSelector);
    end
  end
end
%=============================================================================
function tf = isGroupedVariableSelector(selector)
  tf = iscell(selector) && ~iscellstr(selector);
end
%=============================================================================
function [xValues, names, values, legendLabels, displayVariables] = ...
    groupedTableSeries(tables, displaySelector, xVariableName)
  if length(tables) ~= 1
    error('Nelson:stackedplot:groupedVariableSelectorsAre', 'Grouped variable selectors are supported only for one table input.');
  end
  tbl = tables{1};
  variableNames = tbl.Properties.VariableNames;
  xValues = {};
  names = {};
  values = {};
  legendLabels = {};
  displayVariables = {};
  tableXVariableName = xVariableForTable(xVariableName, 1, 1);
  for k = 1:length(displaySelector)
    groupNames = resolveVariableSelector(displaySelector{k}, variableNames);
    panelX = {};
    panelY = {};
    panelLegend = {};
    for j = 1:length(groupNames)
      name = groupNames{j};
      if shouldSkipVariable(tbl, name, tableXVariableName, true)
        continue
      end
      validateVariableName(name, variableNames);
      value = tbl.(name);
      if isStackedPlotValue(value)
        x = tableXData(tbl, tableXVariableName);
        [panelX, panelY, panelLegend] = appendValueSeries(panelX, panelY, panelLegend, ...
          x, value, name);
      end
    end
    if ~isempty(panelY)
      names{end + 1} = groupedDisplayName(groupNames);
      values{end + 1} = panelY;
      xValues{end + 1} = panelX;
      legendLabels{end + 1} = panelLegend;
      displayVariables{end + 1} = groupNames;
    end
  end
end
%=============================================================================
function label = groupedDisplayName(names)
  if length(names) == 1
    label = names{1};
  else
    label = names{1};
    for k = 2:length(names)
      label = [label, ', ', names{k}];
    end
  end
end
%=============================================================================
function [xValues, names, values, legendLabels, displayVariables] = ...
    combinedTableSeries(tables, candidateNames, xVariableName, explicitSelector)
  xValues = {};
  names = {};
  values = {};
  legendLabels = {};
  displayVariables = {};
  for k = 1:length(candidateNames)
    name = candidateNames{k};
    panelX = {};
    panelY = {};
    panelLegend = {};
    for t = 1:length(tables)
      tbl = tables{t};
      tableXVariableName = xVariableForTable(xVariableName, t, length(tables));
      if shouldSkipVariable(tbl, name, tableXVariableName, explicitSelector)
        continue
      end
      validateVariableName(name, tbl.Properties.VariableNames);
      value = tbl.(name);
      if isStackedPlotValue(value)
        x = tableXData(tbl, tableXVariableName);
        [panelX, panelY, panelLegend] = appendValueSeries(panelX, panelY, panelLegend, ...
          x, value, sourceLegendLabel(t, length(tables), name));
      end
    end
    if ~isempty(panelY)
      names{end + 1} = name;
      values{end + 1} = panelY;
      xValues{end + 1} = panelX;
      legendLabels{end + 1} = panelLegend;
      displayVariables{end + 1} = name;
    end
  end
end
%=============================================================================
function [xValues, names, values, legendLabels, displayVariables] = ...
    separateTableSeries(tables, candidateNames, xVariableName, explicitSelector)
  xValues = {};
  names = {};
  values = {};
  legendLabels = {};
  displayVariables = {};
  for t = 1:length(tables)
    tbl = tables{t};
    tableXVariableName = xVariableForTable(xVariableName, t, length(tables));
    for k = 1:length(candidateNames)
      name = candidateNames{k};
      if shouldSkipVariable(tbl, name, tableXVariableName, explicitSelector)
        continue
      end
      validateVariableName(name, tbl.Properties.VariableNames);
      value = tbl.(name);
      if isStackedPlotValue(value)
        x = tableXData(tbl, tableXVariableName);
        [panelX, panelY, panelLegend] = appendValueSeries({}, {}, {}, x, value, name);
        names{end + 1} = separatedDisplayName(name, t, length(tables));
        values{end + 1} = panelY;
        xValues{end + 1} = panelX;
        legendLabels{end + 1} = panelLegend;
        if ~any(strcmp(displayVariables, name))
          displayVariables{end + 1} = name;
        end
      end
    end
  end
end
%=============================================================================
function tf = shouldSkipVariable(tbl, name, xVariableName, explicitSelector)
  tf = ~any(strcmp(name, tbl.Properties.VariableNames));
  if tf
    return
  end
  tf = ~explicitSelector && ~isempty(xVariableName) && strcmp(name, xVariableName);
end
%=============================================================================
function x = tableXData(tbl, xVariableName)
  if ~isempty(xVariableName)
    validateVariableName(xVariableName, tbl.Properties.VariableNames);
    x = stackedplotValueToNumeric(tbl.(xVariableName));
  elseif istimetable(tbl)
    x = rowTimesToNumeric(tbl.Properties.RowTimes);
  else
    x = (1:height(tbl))';
  end
  x = x(:);
end
%=============================================================================
function xLabelName = defaultTableXLabel(tbl, xVariableName)
  if ~isempty(xVariableName)
    if iscellstr(xVariableName)
      xVariableName = xVariableName{1};
    end
    validateVariableName(xVariableName, tbl.Properties.VariableNames);
    xLabelName = xVariableName;
  elseif istimetable(tbl)
    xLabelName = timetableRowTimeName(tbl);
  else
    xLabelName = 'Row';
  end
end
%=============================================================================
function validateXVariableForTables(tables, xVariableName)
  if isempty(xVariableName)
    return
  end
  if istimetable(tables{1})
    error('Nelson:stackedplot:xVariableIsSupportedOnly', 'XVariable is supported only for table inputs.');
  end
  if iscellstr(xVariableName) && length(xVariableName) > 1 && length(xVariableName) ~= length(tables)
    error('Nelson:stackedplot:invalidXVariable', 'XVariable must contain one name or one name per table.');
  end
end
%=============================================================================
function name = xVariableForTable(xVariableName, index, tableCount)
  name = xVariableName;
  if iscellstr(xVariableName)
    if length(xVariableName) == tableCount
      name = xVariableName{index};
    else
      name = xVariableName{1};
    end
  end
end
%=============================================================================
function [panelX, panelY, panelLegend] = appendValueSeries(panelX, panelY, panelLegend, ...
    x, value, baseLabel)
  y = stackedplotValueToNumeric(value);
  if isvector(y)
    y = y(:);
  end
  if size(y, 1) ~= length(x)
    error('Nelson:stackedplot:invalidXAndY', 'X and Y data must have compatible lengths.');
  end
  for c = 1:size(y, 2)
    panelX{end + 1} = x;
    panelY{end + 1} = y(:, c);
    if size(y, 2) == 1
      panelLegend{end + 1} = baseLabel;
    else
      panelLegend{end + 1} = [baseLabel, ' Column ', int2str(c)];
    end
  end
end
%=============================================================================
function label = sourceLegendLabel(index, count, name)
  if count <= 1
    label = name;
  else
    label = ['Input ', int2str(index)];
  end
end
%=============================================================================
function name = separatedDisplayName(name, index, count)
  if count > 1
    name = [name, ' (', int2str(index), ')'];
  end
end
%=============================================================================
function [xValues, names, values, legendLabels, xLabelName, displayVariables] = ...
    matrixStackedplotData(x, y)
  y = stackedplotValueToNumeric(y);
  if isempty(x)
    x = (1:size(y, 1))';
    xLabelName = '';
  else
    x = stackedplotValueToNumeric(x);
    xLabelName = '';
  end
  x = x(:);
  if isvector(y)
    y = y(:);
  end

  xValues = {};
  names = {};
  values = {};
  legendLabels = {};
  for k = 1:size(y, 2)
    name = ['Column ', int2str(k)];
    names{end + 1} = name;
    values{end + 1} = {y(:, k)};
    xValues{end + 1} = {x};
    legendLabels{end + 1} = {name};
  end
  displayVariables = {};
end
%=============================================================================
function [axesHandles, lineProperties, axesProperties] = ...
    renderStackedplotAxes(xValues, names, values, legendLabels, xLabelName, options)
  axesHandles = [];
  lineProperties = [];
  axesProperties = [];
  labelHandles = [];
  axesPositions = stackedplotAxesPositions(length(values));
  if useStackedplotRenderBuiltin(options)
    [axesHandles, labelHandles, lineHandlesByPanel] = __stackedplot_render__(...
      options.Parent, axesPositions, xValues, values, names, xLabelName, options.Title);
    for k = 1:length(values)
      applyLineOptions(lineHandlesByPanel{k}, options.LineOptionNames, options.LineOptionValues);
      applyStackedplotPanelYLimits(axesHandles(k), values{k});
      defaultColors = stackedplotPanelColors(axesHandles(k), length(values{k}), options);
      lineProperties = appendObjectProperty(lineProperties, ...
        stackedplotLineProperties(options, defaultColors));
      axesProperties = appendObjectProperty(axesProperties, stackedplotAxesProperties(legendLabels{k}));
    end
    applyStackedplotAxesFonts(axesHandles, options);
    applyStackedplotXAxisVisibility(axesHandles);
    adjustStackedplotLayout(axesHandles, labelHandles, names, values, options);
    return
  end
  for k = 1:length(values)
    ax = createStackedplotAxes(options.Parent, axesPositions(k, :));
    lineHandles = [];
    defaultColors = stackedplotPanelColors(ax, length(values{k}), options);
    for j = 1:length(values{k})
      lineHandles = [lineHandles; line('Parent', ax, 'XData', xValues{k}{j}, ...
        'YData', values{k}{j}, 'Color', defaultColors(j, :))];
    end
    applyLineOptions(lineHandles, options.LineOptionNames, options.LineOptionValues);
    applyStackedplotPanelYLimits(ax, values{k});
    renderStackedplotEvents(ax, options.EventData, options.EventsVisible);
    set(ax, 'Box', 'off');
    labelHandles = [labelHandles; stackedplotLabel(ax, names{k})];
    if k ~= length(values)
      set(ax, 'XTick', []);
      set(ax, 'XTickLabel', {});
    else
      xlabel(ax, xLabelName);
    end
    if k == 1
      title(ax, options.Title);
    end
    axesHandles = [axesHandles; ax];
    lineProperties = appendObjectProperty(lineProperties, ...
      stackedplotLineProperties(options, defaultColors));
    axesProperties = appendObjectProperty(axesProperties, stackedplotAxesProperties(legendLabels{k}));
  end
  applyStackedplotAxesFonts(axesHandles, options);
  applyStackedplotXAxisVisibility(axesHandles);
  adjustStackedplotLayout(axesHandles, labelHandles, names, values, options);
end
%=============================================================================
function applyStackedplotXAxisVisibility(axesHandles)
  % Only the bottom panel shows the shared x-axis (ruler line, ticks and
  % labels). Upper panels hide their x-axis so no baseline is drawn between
  % panels, but they keep the same x-ticks so the vertical grid still spans
  % every panel.
  n = length(axesHandles);
  if n == 0
    return
  end
  sharedXTick = get(axesHandles(n), 'XTick');
  for k = 1:n
    ax = axesHandles(k);
    if ~isempty(sharedXTick)
      set(ax, 'XTick', sharedXTick);
    end
    if k < n
      set(ax, 'XTickLabel', {});
      xAxisRuler = get(ax, 'XAxis');
      if ~isempty(xAxisRuler)
        set(xAxisRuler, 'Visible', 'off');
      end
    end
  end
end
%=============================================================================
function applyStackedplotAxesFonts(axesHandles, options)
  % stackedplot uses a compact default font. Tick labels follow the axes
  % font, while the x-label and title use the usual 1.1 label multiplier.
  fontSize = stackedplotInitialFontSize(options);
  labelFontSize = fontSize * 1.1;
  for k = 1:length(axesHandles)
    ax = axesHandles(k);
    set(ax, 'FontSize', fontSize);
    xLabelHandle = get(ax, 'XLabel');
    if ~isempty(xLabelHandle) && isgraphics(xLabelHandle)
      set(xLabelHandle, 'FontSize', labelFontSize);
    end
    titleHandle = get(ax, 'Title');
    if ~isempty(titleHandle) && isgraphics(titleHandle)
      set(titleHandle, 'FontSize', labelFontSize);
    end
  end
end
%=============================================================================
function tf = useStackedplotRenderBuiltin(options)
  tf = isempty(options.EventData) || strcmp(options.EventsVisible, 'off');
end
%=============================================================================
function ax = createStackedplotAxes(parent, position)
  if isempty(parent)
    ax = axes('Position', position);
  else
    ax = axes(parent, 'Position', position);
  end
end
%=============================================================================
function applyStackedplotPanelYLimits(ax, panelValues)
  % Fit each panel's Y-axis tightly to its data range so that the tick
  % density matches the values shown, instead of rounding the limits
  % outward (which produces coarser ticks). An explicit YLimits supplied
  % through AxesProperties still overrides this default afterwards.
  numericValues = [];
  for k = 1:length(panelValues)
    value = panelValues{k};
    if isnumeric(value) || islogical(value)
      numericValues = [numericValues; double(value(:))];
    end
  end
  numericValues = numericValues(isfinite(numericValues));
  if isempty(numericValues)
    return
  end
  lowerLimit = min(numericValues);
  upperLimit = max(numericValues);
  if upperLimit > lowerLimit
    ax.YLim = [lowerLimit, upperLimit];
  end
end
%=============================================================================
function options = stackedplotDefaultOptions()
  options = struct();
  options.Parent = [];
  options.DisplayVariables = [];
  options.DisplayLabels = [];
  options.XVariable = '';
  options.XLabel = [];
  options.XData = [];
  options.YData = [];
  options.XLimits = [];
  options.CombineMatchingNames = true;
  options.EventsVisible = 'on';
  options.GridVisible = 'off';
  options.Title = '';
  options.LegendLabels = [];
  options.LegendVisible = 'off';
  options.LegendOrientation = 'horizontal';
  options.LineProperties = [];
  options.AxesProperties = [];
  options.LineOptionNames = {};
  options.LineOptionValues = {};
  options.ChartPropertyNames = {};
  options.ChartPropertyValues = {};
  options.EventData = [];
end
%=============================================================================
function options = parseStackedplotOptions(inputArguments, firstNameValue, options)
  k = firstNameValue;
  while k <= length(inputArguments)
    if k == length(inputArguments) || ~ischar(inputArguments{k})
      error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
    end
    name = normalizedName(inputArguments{k});
    value = inputArguments{k + 1};
    switch name
      case 'displayvariables'
        options.DisplayVariables = value;
      case 'displaylabels'
        options.DisplayLabels = value;
      case 'xvariable'
        options.XVariable = xVariableSelector(value);
      case 'xlabel'
        options.XLabel = scalarName(value);
      case 'xlimits'
        options.XLimits = stackedplotValueToNumeric(value);
      case 'combinematchingnames'
        options.CombineMatchingNames = logicalScalar(value);
      case 'eventsvisible'
        options.EventsVisible = onOffValue(value);
      case 'gridvisible'
        options.GridVisible = onOffValue(value);
      case 'title'
        options.Title = scalarName(value);
      case 'legendlabels'
        options.LegendLabels = value;
      case 'legendvisible'
        options.LegendVisible = onOffValue(value);
      case 'legendorientation'
        options.LegendOrientation = scalarName(value);
      case 'lineproperties'
        options.LineProperties = value;
      case 'axesproperties'
        options.AxesProperties = value;
      case {'color', 'linestyle', 'linewidth', 'marker', 'markersize', ...
          'markeredgecolor', 'markerfacecolor'}
        options.LineOptionNames{end + 1} = inputArguments{k};
        options.LineOptionValues{end + 1} = value;
        options = addChartProperty(options, inputArguments{k}, value);
      case {'fontname', 'fontsize'}
        options = addChartProperty(options, inputArguments{k}, value);
      otherwise
        error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'stackedplot');
    end
    k = k + 2;
  end
end
%=============================================================================
function tf = isNameValueStart(inputArguments, index)
  tf = false;
  if index > length(inputArguments) || ~ischar(inputArguments{index})
    return
  end
  names = {'displayvariables', 'displaylabels', 'xvariable', 'xlabel', ...
    'xlimits', 'combinematchingnames', 'eventsvisible', 'gridvisible', 'title', ...
    'legendlabels', 'legendvisible', 'legendorientation', 'lineproperties', ...
    'axesproperties', 'color', 'linestyle', 'linewidth', 'marker', 'markersize', ...
    'markeredgecolor', 'markerfacecolor', 'fontname', 'fontsize'};
  tf = any(strcmp(normalizedName(inputArguments{index}), names));
end
%=============================================================================
function names = resolveVariableSelector(selector, variableNames)
  if isempty(selector)
    names = variableNames;
  elseif ischar(selector)
    names = {selector};
  elseif iscellstr(selector)
    names = selector;
  elseif isnumeric(selector)
    names = variableNames(selector);
  elseif islogical(selector)
    names = variableNames(selector);
  else
    error('Nelson:stackedplot:invalidVariableSelector', 'Invalid variable selector.');
  end
  names = names(:)';
end
%=============================================================================
function labels = normalizeLabels(labels, n)
  if ischar(labels)
    labels = {labels};
  elseif ~iscellstr(labels)
    error('Nelson:stackedplot:invalidDisplayLabels', 'Invalid display labels.');
  end
  labels = labels(:)';
  if length(labels) ~= n
    error('Nelson:stackedplot:invalidDisplayLabelsLength', 'Display labels length must match the number of plots.');
  end
end
%=============================================================================
function validateVariableName(name, variableNames)
  if ~any(strcmp(name, variableNames))
    error('Nelson:stackedplot:invalidVariableName', 'Invalid variable name.');
  end
end
%=============================================================================
function names = removeVariableName(names, name)
  names = names(~strcmp(names, name));
end
%=============================================================================
function value = stackedplotValueToNumeric(value)
  if isa(value, 'datetime')
    value = datenum(value);
  elseif isa(value, 'duration')
    value = seconds(value);
  else
    value = double(value);
  end
end
%=============================================================================
function tf = isStackedPlotValue(value)
  tf = (isnumeric(value) || islogical(value) || isa(value, 'datetime') || isa(value, 'duration')) ...
    && ismatrix(value) && ~isempty(value);
end
%=============================================================================
function tf = isLineSpecArgument(value)
  tf = false;
  if ~(ischar(value) || isStringScalar(value))
    return
  end
  [~, ~, ~, msg] = colstyle(value, 'plot', false);
  tf = isempty(msg);
end
%=============================================================================
function x = rowTimesToNumeric(rowTimes)
  if isa(rowTimes, 'datetime')
    x = datenum(rowTimes);
  elseif isa(rowTimes, 'duration')
    x = seconds(rowTimes);
  else
    x = rowTimes;
  end
  x = x(:);
end
%=============================================================================
function eventData = stackedplotEventData(tables)
  eventData = [];
  for k = 1:length(tables)
    tbl = tables{k};
    if ~istimetable(tbl) || ~isfield(tbl.Properties, 'Events')
      continue
    end
    events = tbl.Properties.Events;
    if ~isa(events, 'eventtable') || height(events) == 0
      continue
    end
    current = eventTableData(events);
    if isempty(current)
      continue
    end
    if isempty(eventData)
      eventData = current;
    else
      eventData.X = [eventData.X; current.X];
      eventData.Width = [eventData.Width; current.Width];
      eventData.Labels = [eventData.Labels; current.Labels];
    end
  end
end
%=============================================================================
function eventData = eventTableData(events)
  props = events.Properties;
  eventTimes = props.RowTimes;
  eventData = struct();
  eventData.X = rowTimesToNumeric(eventTimes);
  eventData.Width = eventWidthsToNumeric(events, props.EventLengthsVariable, ...
    props.EventEndsVariable, eventTimes);
  eventData.Labels = eventLabels(events, props.EventLabelsVariable, length(eventData.X));
  keep = isfinite(eventData.X) & isfinite(eventData.Width);
  eventData.X = eventData.X(keep);
  eventData.Width = eventData.Width(keep);
  eventData.Labels = eventData.Labels(keep);
  if isempty(eventData.X)
    eventData = [];
  end
end
%=============================================================================
function labels = eventLabels(events, labelName, count)
  labels = repmat({''}, count, 1);
  if isempty(labelName)
    return
  end
  value = events.(labelName);
  if isstring(value)
    labels = cellstr(value(:));
  elseif iscellstr(value)
    labels = value(:);
  elseif ischar(value)
    labels = cellstr(value);
    labels = labels(:);
  else
    labels = cellstr(string(value(:)));
  end
  if length(labels) ~= count
    labels = repmat({''}, count, 1);
  end
end
%=============================================================================
function widths = eventWidthsToNumeric(events, lengthName, endName, eventTimes)
  widths = zeros(length(rowTimesToNumeric(eventTimes)), 1);
  if ~isempty(endName)
    ends = events.(endName);
    widths = rowTimesToNumeric(ends) - rowTimesToNumeric(eventTimes);
    widths = max(widths(:), 0);
    return
  end
  if isempty(lengthName)
    return
  end
  lengths = events.(lengthName);
  if isa(lengths, 'duration')
    widths = seconds(lengths(:));
    if isa(eventTimes, 'datetime')
      widths = widths ./ 86400;
    end
  else
    widths = double(lengths(:));
  end
  widths = max(widths, 0);
end
%=============================================================================
function renderStackedplotEvents(ax, eventData, eventsVisible)
  if isempty(eventData) || strcmp(eventsVisible, 'off')
    return
  end
  yLimits = ax.YLim;
  if isempty(yLimits) || length(yLimits) ~= 2 || yLimits(1) == yLimits(2)
    yLimits = [0 1];
  end
  colorOrder = ax.ColorOrder;
  if isempty(colorOrder)
    colorOrder = [0.8 0.8 0.8];
  end
  thinWidth = stackedplotEventThinWidth(ax, eventData.X);
  labels = unique(eventData.Labels, 'stable');
  for k = 1:length(eventData.X)
    x1 = eventData.X(k);
    x2 = x1 + eventData.Width(k);
    if x2 == x1
      x1 = x1 - thinWidth / 2;
      x2 = x2 + thinWidth / 2;
    end
    color = stackedplotEventColor(eventData.Labels{k}, labels, colorOrder);
    patch(ax, [x1 x2 x2 x1], [yLimits(1) yLimits(1) yLimits(2) yLimits(2)], color, ...
      'EdgeColor', 'none', ...
      'FaceAlpha', 0.18, ...
      'Tag', 'StackedplotEvent');
  end
  ax.YLim = yLimits;
end
%=============================================================================
function width = stackedplotEventThinWidth(ax, x)
  xLimits = ax.XLim;
  if isempty(xLimits) || length(xLimits) ~= 2 || xLimits(1) == xLimits(2)
    xLimits = [min(x), max(x)];
  end
  span = abs(xLimits(2) - xLimits(1));
  if span == 0 || ~isfinite(span)
    span = 1;
  end
  width = span * 0.003;
end
%=============================================================================
function color = stackedplotEventColor(label, labels, colorOrder)
  index = find(strcmp(labels, label), 1);
  if isempty(index)
    index = 1;
  end
  color = colorOrder(mod(index - 1, size(colorOrder, 1)) + 1, :);
end
%=============================================================================
function positions = stackedplotAxesPositions(n)
  % Generic stacked-panel layout. The panel column is anchored at a fixed
  % left margin and width (the display labels live in the left margin, they
  % do not shift the panels). Panels share a common column between a fixed
  % bottom and top edge; the vertical gap between two panels is a constant
  % fraction of the panel pitch, so the layout stays proportional for any
  % number of panels.
  left = 0.13;
  width = 0.775;
  bottom = 0.11;
  topEdge = 0.925;
  gapFraction = 0.1;
  totalHeight = topEdge - bottom;
  pitch = totalHeight / (n - gapFraction);
  height = pitch * (1 - gapFraction);
  gap = pitch - height;
  positions = zeros(n, 4);
  for k = 1:n
    y = topEdge - k * height - (k - 1) * gap;
    positions(k, :) = [left, y, width, height];
  end
end
%=============================================================================
function name = timetableRowTimeName(tbl)
  name = 'Time';
  if isfield(tbl.Properties, 'DimensionNames') && ~isempty(tbl.Properties.DimensionNames)
    name = tbl.Properties.DimensionNames{1};
  end
end
%=============================================================================
function h = stackedplotLabel(ax, label)
  h = text(ax, -0.035, 0.5, label, ...
    'Units', 'normalized', ...
    'HorizontalAlignment', 'right', ...
    'VerticalAlignment', 'middle', ...
    'Rotation', 0);
end
%=============================================================================
function adjustStackedplotLayout(axesHandles, labelHandles, names, values, options)
  if isempty(axesHandles) || isempty(labelHandles)
    return
  end
  % The panel column keeps a fixed right margin. Its left edge stays at the
  % base margin when the display labels and y-tick labels fit inside it, and
  % only shifts right (shrinking the panels) when the labels are too wide to
  % fit. The display labels sit in the left margin, right aligned just left
  % of the y-tick labels and vertically centred. Text widths are measured
  % with the real font metrics so the layout is independent of the actual
  % label strings.
  parent = axesHandles(1).Parent;
  parentPosition = parent.Position;
  figureWidth = stackedplotTextReferenceWidth(parentPosition);
  fontSize = stackedplotInitialFontSize(options);
  labelFontSize = fontSize * 1.1;
  baseLeft = 0.13;
  right = 0.095;
  outerLeftPixels = 6;
  labelTickGapPixels = 4;
  tickAxisGapPixels = 4;
  minimumAxesWidth = 0.2;
  labelWidthPixels = maxStackedplotMeasuredWidth(axesHandles(1), names, labelFontSize);
  tickWidthPixels = maxStackedplotMeasuredTickWidth(axesHandles(1), values, fontSize);
  leftPixels = outerLeftPixels + labelWidthPixels + labelTickGapPixels + ...
    tickWidthPixels + tickAxisGapPixels;
  left = leftPixels / figureWidth;
  left = min(max(left, baseLeft), 1 - right - minimumAxesWidth);
  width = max(1 - left - right, minimumAxesWidth);
  axesWidthPixels = max(width * figureWidth, 1);
  labelX = -(tickWidthPixels + labelTickGapPixels) / axesWidthPixels;
  for k = 1:length(axesHandles)
    position = axesHandles(k).Position;
    position(1) = left;
    position(3) = width;
    axesHandles(k).Position = position;
  end
  for k = 1:length(labelHandles)
    set(labelHandles(k), 'FontSize', labelFontSize);
    labelPosition = labelHandles(k).Position;
    labelPosition(1) = labelX;
    labelHandles(k).Position = labelPosition;
  end
end
%=============================================================================
function width = maxStackedplotMeasuredWidth(ax, labels, fontSize)
  width = 0;
  if ischar(labels)
    labels = {labels};
  end
  for k = 1:numel(labels)
    label = labels{k};
    if ~ischar(label)
      label = char(string(label));
    end
    width = max(width, stackedplotMeasuredTextWidth(ax, label, fontSize));
  end
end
%=============================================================================
function width = maxStackedplotMeasuredTickWidth(ax, values, fontSize)
  width = 0;
  for k = 1:length(values)
    labels = stackedplotEstimatedTickLabels(values{k});
    for j = 1:length(labels)
      width = max(width, stackedplotMeasuredTextWidth(ax, labels{j}, fontSize));
    end
  end
end
%=============================================================================
function width = stackedplotMeasuredTextWidth(ax, label, fontSize)
  % Measure the pixel width of a string using the real font metrics, then
  % remove the probe so it never affects the axes.
  probe = text(ax, 0, 0, label, ...
    'Units', 'pixels', ...
    'FontSize', fontSize, ...
    'Visible', 'off');
  extent = get(probe, 'Extent');
  delete(probe);
  width = extent(3);
end
%=============================================================================
function width = stackedplotTextReferenceWidth(parentPosition)
  width = max(parentPosition(3), 1);
  try
    screenSize = get(0, 'ScreenSize');
    if width <= 1.5 && length(screenSize) >= 3
      width = max(width * screenSize(3), 1);
    end
  catch
  end
end
%=============================================================================
function fontSize = stackedplotInitialFontSize(options)
  fontSize = stackedplotOptionValue(options.ChartPropertyNames, options.ChartPropertyValues, ...
    'FontSize', 8);
end
%=============================================================================
function labels = stackedplotEstimatedTickLabels(values)
  numericValues = [];
  for k = 1:length(values)
    value = values{k};
    if isnumeric(value) || islogical(value)
      numericValues = [numericValues; double(value(:))];
    end
  end
  numericValues = numericValues(isfinite(numericValues));
  if isempty(numericValues)
    labels = {'0'};
    return
  end
  minimumValue = min(numericValues);
  maximumValue = max(numericValues);
  labels = {num2str(minimumValue, 4), num2str(maximumValue, 4)};
  if minimumValue <= 0 && maximumValue >= 0
    labels{end + 1} = '0';
  end
end
%=============================================================================
function applyLineOptions(lineHandles, names, values)
  for k = 1:length(names)
    set(lineHandles, names{k}, values{k});
  end
end
%=============================================================================
function properties = stackedplotLineProperties(options, defaultColors)
  color = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'Color', defaultColors);
  markerFaceColor = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'MarkerFaceColor', 'none');
  markerEdgeColor = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'MarkerEdgeColor', color);
  lineStyle = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'LineStyle', '-');
  lineWidth = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'LineWidth', 0.5);
  marker = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'Marker', 'none');
  markerSize = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
    'MarkerSize', 6);
  properties = StackedLineProperties(color, ...
    markerFaceColor, ...
    markerEdgeColor, ...
    lineStyle, ...
    lineWidth, ...
    marker, ...
    markerSize, ...
    'plot');
end
%=============================================================================
function colors = stackedplotPanelColors(ax, lineCount, options)
  explicitColor = stackedplotHasOption(options.LineOptionNames, 'Color');
  if explicitColor
    color = stackedplotOptionValue(options.LineOptionNames, options.LineOptionValues, ...
      'Color', [0.0660 0.4430 0.7450]);
    if isnumeric(color) && ismatrix(color) && size(color, 1) == lineCount && size(color, 2) == 3
      colors = color;
    else
      colors = repmat([0.0660 0.4430 0.7450], lineCount, 1);
    end
    return
  end
  colorOrder = ax.ColorOrder;
  if isempty(colorOrder)
    colors = zeros(lineCount, 3);
    return
  end
  colors = zeros(lineCount, 3);
  colorCount = size(colorOrder, 1);
  for k = 1:lineCount
    index = mod(k - 1, colorCount) + 1;
    colors(k, :) = colorOrder(index, :);
  end
end
%=============================================================================
function tf = stackedplotHasOption(names, name)
  tf = false;
  for k = 1:length(names)
    if strcmpi(names{k}, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function properties = stackedplotAxesProperties(labels)
  properties = StackedAxesProperties(...
    [], ...
    'linear', ...
    labels, ...
    'northeast', ...
    'off', ...
    'off');
end
%=============================================================================
function value = stackedplotOptionValue(names, values, name, defaultValue)
  value = defaultValue;
  for k = 1:length(names)
    if strcmpi(names{k}, name)
      value = values{k};
    end
  end
end
%=============================================================================
function properties = appendObjectProperty(properties, value)
  if isempty(properties)
    properties = value;
  else
    properties(end + 1, 1) = value;
  end
end
%=============================================================================
function lineHandles = stackedplotLineChildren(ax)
  lineHandles = [];
  % Children run top first, so they are walked the other way up to hand the
  % lines back in the order they were plotted.
  children = flipud(ax.Children(:));
  for k = 1:length(children)
    if isgraphics(children(k), 'line')
      lineHandles = [lineHandles; children(k)];
    end
  end
end
%=============================================================================
function options = applyLineSpec(options, lineSpec)
  [lineStyle, colorSpec, markerSpec, msg] = colstyle(lineSpec, 'plot', false);
  if ~isempty(msg)
    error('Nelson:stackedplot:invalidLineSpec', msg.message);
  end
  if ~isempty(colorSpec)
    options.LineOptionNames{end + 1} = 'Color';
    options.LineOptionValues{end + 1} = colorSpec;
    options = addChartProperty(options, 'Color', colorSpec);
    options.LineOptionNames{end + 1} = 'MarkerEdgeColor';
    options.LineOptionValues{end + 1} = colorSpec;
    options = addChartProperty(options, 'MarkerEdgeColor', colorSpec);
  end
  if ~isempty(markerSpec)
    options.LineOptionNames{end + 1} = 'Marker';
    options.LineOptionValues{end + 1} = markerSpec;
    options = addChartProperty(options, 'Marker', markerSpec);
  end
  if ~isempty(lineStyle)
    options.LineOptionNames{end + 1} = 'LineStyle';
    options.LineOptionValues{end + 1} = lineStyle;
    options = addChartProperty(options, 'LineStyle', lineStyle);
  end
end
%=============================================================================
function value = optionOrDefault(optionValue, defaultValue)
  if isempty(optionValue)
    value = defaultValue;
  else
    value = optionValue;
  end
end
%=============================================================================
function options = addChartProperty(options, name, value)
  options.ChartPropertyNames{end + 1} = chartPropertyName(name);
  options.ChartPropertyValues{end + 1} = value;
end
%=============================================================================
function applyStackedplotChartProperties(s, options)
  for k = 1:length(options.ChartPropertyNames)
    s.(options.ChartPropertyNames{k}) = options.ChartPropertyValues{k};
  end
end
%=============================================================================
function name = chartPropertyName(name)
  switch normalizedName(name)
    case 'color'
      name = 'Color';
    case 'linestyle'
      name = 'LineStyle';
    case 'linewidth'
      name = 'LineWidth';
    case 'marker'
      name = 'Marker';
    case 'markersize'
      name = 'MarkerSize';
    case 'markeredgecolor'
      name = 'MarkerEdgeColor';
    case 'markerfacecolor'
      name = 'MarkerFaceColor';
    case 'fontname'
      name = 'FontName';
    case 'fontsize'
      name = 'FontSize';
    otherwise
      error('Nelson:graphics:unsupportedProperty', 'Unsupported "%s" option.', 'stackedplot');
  end
end
%=============================================================================
function applyInitialAxesProperties(s)
  axesProperties = s.AxesProperties;
  if ~(isstruct(axesProperties) || isobject(axesProperties))
    return
  end
  axesCount = min(length(s.Children), length(axesProperties));
  for k = 1:axesCount
    axesLegendOn = hasStackedplotProperty(axesProperties, 'LegendVisible') && ...
      strcmp(onOffValue(axesProperties(k).LegendVisible), 'on');
    chartLegendOn = strcmp(onOffValue(s.LegendVisible), 'on');
    if axesLegendOn || chartLegendOn
      labels = axesProperties(k).LegendLabels;
      if chartLegendOn && ~isempty(s.LegendLabels)
        labels = s.LegendLabels;
      end
      if isempty(labels)
        labels = {};
      end
      if hasStackedplotProperty(axesProperties, 'LegendLocation')
        location = axesProperties(k).LegendLocation;
      else
        location = 'northeast';
      end
      legend(s.Children(k), labels, 'Location', location);
    end
  end
end
%=============================================================================
function tf = hasStackedplotProperty(value, name)
  if isstruct(value)
    tf = isfield(value, name);
  else
    tf = isprop(value, name);
    tf = any(tf(:));
  end
end
%=============================================================================
function value = scalarName(value)
  if iscellstr(value) && isscalar(value)
    value = value{1};
  elseif ~ischar(value)
    error('Nelson:stackedplot:characterRowVectorExpected', 'Character row vector expected.');
  end
end
%=============================================================================
function value = xVariableSelector(value)
  if iscellstr(value)
    value = value(:)';
    if isscalar(value)
      value = value{1};
    end
  elseif ~ischar(value)
    error('Nelson:stackedplot:characterRowVectorOrCellExpected', 'Character row vector or cell array of character row vectors expected.');
  end
end
%=============================================================================
function value = onOffValue(value)
  % Graphics on/off properties read back as nelson.lang.OnOffSwitchState
  % enumerations, so coerce any non char/cellstr value to a character row
  % vector before validating it.
  if ~ischar(value) && ~iscellstr(value)
    value = char(value);
  end
  value = scalarName(value);
  value = lower(value);
  if ~(strcmp(value, 'on') || strcmp(value, 'off'))
    error('Nelson:stackedplot:invalidOnOffValue', 'on/off value expected.');
  end
end
%=============================================================================
function value = logicalScalar(value)
  if islogical(value) && isscalar(value)
    return
  end
  if isnumeric(value) && isscalar(value)
    value = logical(value);
    return
  end
  if ischar(value)
    switch lower(value)
      case {'true', 'on', '1'}
        value = true;
      case {'false', 'off', '0'}
        value = false;
      otherwise
        error('Nelson:stackedplot:logicalScalarExpected', 'Logical scalar expected.');
    end
    return
  end
  error('Nelson:stackedplot:logicalScalarExpected', 'Logical scalar expected.');
end
%=============================================================================
function name = normalizedName(name)
  name = lower(name);
  name = strrep(name, '_', '');
  name = strrep(name, ' ', '');
end
%=============================================================================
