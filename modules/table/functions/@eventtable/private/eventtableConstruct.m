%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [TT, vars] = eventtableConstruct(args)
  % Timetable holding the event data, and the event variable names.
  opts = parseArguments(args);
  first = args{1};
  isTimetableInput = istimetable(first);
  if isTimetableInput
    rowTimes = first.Properties.RowTimes;
  elseif (isa(first, 'datetime') || isa(first, 'duration')) && ~isequal(size(first), [0 0])
    rowTimes = first(:);
  else
    error('Nelson:eventtable:invalidEventTimes', 'First input must either be a timetable or it must be a datetime or duration vector.');
  end
  checkOptions(opts, isTimetableInput);
  [newNames, newValues, newFields] = eventOptionVariables(opts, isTimetableInput, rowTimes);
  vars = eventtableNoVariables();
  fields = fieldnames(vars);
  if isTimetableInput
    TT = first;
    newNames = uniqueNames(newNames, TT.Properties.VariableNames);
    for k = 1:length(newValues)
      TT.(newNames{k}) = newValues{k};
    end
  else
    TT = timetable(rowTimes(:), newValues{:}, 'VariableNames', newNames);
  end
  for k = 1:length(newNames)
    vars.(newFields{k}) = newNames{k};
  end
  st = struct(TT);
  for k = 1:length(fields)
    if isfield(opts.names, fields{k})
      vars.(fields{k}) = eventtableResolveVariable(fields{k}, opts.names.(fields{k}), ...
        st.Table.data, st.Properties.VariableNames, rowTimes);
    end
  end
end
%=============================================================================
function [names, values, fields] = eventOptionVariables(opts, isTimetableInput, rowTimes)
  % Variables created from the EventLabels, EventLengths and EventEnds
  % values; event times alone get default labels.
  if isa(rowTimes, 'datetime')
    nRows = numel(datenum(rowTimes));
  else
    nRows = numel(seconds(rowTimes));
  end
  if ~isTimetableInput && ~isfield(opts.values, 'EventLabels') && nRows > 0
    opts.values.EventLabels = "Event " + string((1:nRows)');
  end
  optionNames = {'EventLabels', 'EventLengths', 'EventEnds'};
  names = {};
  values = {};
  fields = {};
  for k = 1:length(optionNames)
    if isfield(opts.values, optionNames{k})
      field = [optionNames{k}, 'Variable'];
      value = expandOption(opts.values.(optionNames{k}), nRows, optionNames{k});
      eventtableCheckVariableType(field, value, rowTimes);
      names{end + 1} = optionNames{k};
      values{end + 1} = value;
      fields{end + 1} = field;
    end
  end
end
%=============================================================================
function opts = parseArguments(args)
  % The event data come only from the first input: the other arguments are
  % name-value pairs, taken from the end while the name slot holds text.
  opts = struct('values', struct(), 'names', struct());
  optionNames = {'EventLabels', 'EventLabelsVariable', 'EventLengths', ...
    'EventLengthsVariable', 'EventEnds', 'EventEndsVariable'};
  firstName = length(args) + 1;
  while firstName > 2 && isTextScalar(args{firstName - 2})
    firstName = firstName - 2;
  end
  for k = firstName:2:length(args)
    idx = find(strcmpi(char(args{k}), optionNames));
    if isempty(idx)
      error('Nelson:eventtable:invalidArgumentName', 'Invalid argument name ''%s''. Name must be ''EventLabels'', ''EventLabelsVariable'', ''EventLengths'', ''EventLengthsVariable'', ''EventEnds'', or ''EventEndsVariable''.', char(args{k}));
    end
    name = optionNames{idx(1)};
    if endsWith(name, 'Variable')
      opts.names.(name) = args{k + 1};
    else
      opts.values.(name) = args{k + 1};
    end
  end
  if firstName > 2
    if isTextScalar(args{2}) && any(strcmpi(char(args{2}), optionNames))
      error('Nelson:eventtable:nameValuePair', 'Invalid argument list. Make sure name-value arguments include both the name and the value.');
    end
    if firstName > length(args)
      error('Nelson:eventtable:tooManyInputs', 'Invalid argument at position 2. Function requires 0 to 1 positional input(s).');
    end
    error('Nelson:eventtable:tooManyInputs:beforeName', 'Invalid argument at position 2. Function requires 0 to 1 positional input(s) before name ''%s''.', char(args{firstName}));
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = (ischar(value) && isrow(value)) || (isstring(value) && isscalar(value));
end
%=============================================================================
function checkOptions(opts, isTimetableInput)
  variableOptions = fieldnames(opts.names);
  if ~isTimetableInput && ~isempty(variableOptions)
    error('Nelson:eventtable:firstInputMustBeTimetable', '''%s'' name-value pair is only supported when the first input is a timetable.', variableOptions{1});
  end
  kinds = {'labels', 'lengths', 'ends'};
  optionNames = {'EventLabels', 'EventLengths', 'EventEnds'};
  given = false(1, 3);
  for k = 1:3
    hasValue = isfield(opts.values, optionNames{k});
    hasName = isfield(opts.names, [optionNames{k}, 'Variable']);
    if hasValue && hasName
      error('Nelson:eventtable:bothValueAndVariable', 'Specify either event %s or an event %s variable.', kinds{k}, kinds{k});
    end
    given(k) = hasValue || hasName;
  end
  if given(2) && given(3)
    error('Nelson:eventtable:bothLengthsAndEnds', 'Specify either event lengths or event ends but not both.');
  end
end
%=============================================================================
function value = expandOption(value, nRows, optionName)
  if isa(value, 'datetime')
    count = numel(datenum(value));
  elseif isa(value, 'duration')
    count = numel(seconds(value));
  else
    count = numel(value);
  end
  if count == 1
    value = value(ones(nRows, 1));
  elseif count == nRows
    value = value(:);
  else
    error('Nelson:eventtable:dimagree', '''%s'' must either be a scalar or a vector with the same number of elements as the height of the first input.', optionName);
  end
end
%=============================================================================
function names = uniqueNames(names, existing)
  % A new variable does not replace a variable of the same name.
  for k = 1:length(names)
    base = names{k};
    j = 0;
    while any(strcmp(names{k}, existing)) || any(strcmp(names{k}, names(1:k - 1)))
      j = j + 1;
      names{k} = sprintf('%s_%d', base, j);
    end
  end
end
%=============================================================================
