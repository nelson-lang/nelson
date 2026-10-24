%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = piechart(varargin)
  nargoutchk(0, 1);
  narginchk(1, 1000);
  args = parseChartArguments(varargin, 'piechart');
  h = __piechart__(args{:});
  if nargout > 0
    varargout{1} = h;
  else
    varargout = {};
  end
end
%=============================================================================
function args = parseChartArguments(inputArguments, chartName)
  inputArguments = convertStringToCharArgs(inputArguments);
  args = {};
  if isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'figure')
    args = {'Parent', inputArguments{1}};
    inputArguments(1) = [];
  end
  if isempty(inputArguments)
    error('Nelson:graphics:dataExpected', 'Data expected.');
  end
  if isChartPropertyName(inputArguments{1})
    data = [];
  elseif length(inputArguments) >= 2 && isnumeric(inputArguments{1}) && isNameVector(inputArguments{2}) && ~isChartPropertyName(inputArguments{2})
    args = [args, {'Data', inputArguments{1}, 'Names', inputArguments{2}}];
    inputArguments(1:2) = [];
  else
    args = [args, {'Data', inputArguments{1}}];
    inputArguments(1) = [];
  end
  if mod(length(inputArguments), 2) ~= 0
    error('Nelson:graphics:nameValuePair', 'Name-value arguments must be paired.');
  end
  k = 1;
  while k <= length(inputArguments)
    if ~isChartPropertyName(inputArguments{k})
      error('Nelson:piechart:unknownProperty', '%s: Unrecognized property.', chartName);
    end
    args{end + 1} = canonicalChartPropertyName(inputArguments{k});
    args{end + 1} = inputArguments{k + 1};
    k = k + 2;
  end
end
%=============================================================================
function tf = isNameVector(value)
  tf = ischar(value) || isstring(value) || iscell(value);
end
%=============================================================================
function tf = isChartPropertyName(value)
  if ~(ischar(value) || isStringScalar(value))
    tf = false;
    return
  end
  tf = any(strcmpi(convertStringsToChars(value), chartPropertyNames()));
end
%=============================================================================
function name = canonicalChartPropertyName(value)
  value = convertStringsToChars(value);
  names = chartPropertyNames();
  idx = find(strcmpi(value, names), 1);
  name = names{idx};
end
%=============================================================================
function names = chartPropertyNames()
  names = {'Parent', 'Data', 'Names', 'DataVariable', 'NamesVariable', 'Labels', ...
    'LabelStyle', 'StartAngle', 'Direction', 'ExplodedWedges', 'ColorOrder', ...
    'FaceColor', 'EdgeColor', 'FaceAlpha', 'LineWidth', 'FontSize', 'FontName', ...
    'FontColor', 'Interpreter', 'LegendVisible', 'LegendTitle', 'Title', ...
    'DisplayOrder', 'NumDisplayWedges', 'ShowOthers', 'DataMode', 'FontSizeMode', ...
    'LabelsMode', 'LabelStyleMode', 'NamesMode', 'Visible', 'HandleVisibility', ...
    'Units', 'OuterPosition', 'InnerPosition', 'Position', 'PositionConstraint', ...
    'Layout', 'SourceTable'};
end
%=============================================================================
