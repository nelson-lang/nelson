%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TT = syncevents(TT, varargin)
  % Adds the variables of the event table attached to TT: each row of TT
  % gets the values of the events that happen at its row time, and is
  % repeated when several events match it.
  events = TT.Properties.Events;
  if ~isa(events, 'eventtable')
    error('Nelson:eventtable:NoAttachedEvents', _('Timetable must have an event table assigned to the Events property.'));
  end
  [defaultLabel, dataVariables] = parseArguments(varargin);
  eventVars = eventtable.variables(events);
  props = subsref(events, struct('type', '.', 'subs', 'Properties'));
  if isempty(dataVariables)
    names = setdiff(props.VariableNames, {eventVars.EventLengthsVariable, eventVars.EventEndsVariable}, 'stable');
  else
    names = timetableVariableNames(events, dataVariables{1});
  end
  [rows, eventRows] = timetableEventMatches(TT.Properties.RowTimes, props.RowTimes, timetableEventEnds(events));
  ttNames = TT.Properties.VariableNames;
  TT = subsref(TT, struct('type', '()', 'subs', {{rows, ':'}}));
  for k = 1:length(names)
    column = eventColumn(subsref(events, struct('type', '.', 'subs', names{k})), eventRows);
    if strcmp(names{k}, eventVars.EventLabelsVariable) && ~isempty(defaultLabel)
      column(eventRows == 0, :) = labelValue(defaultLabel{1}, column);
    end
    newName = names{k};
    if any(strcmp(newName, ttNames))
      TT = renamevars(TT, newName, [newName, '_tt']);
      newName = [newName, '_et'];
    end
    TT = subsasgn(TT, struct('type', '.', 'subs', newName), column);
    TT = copyVariableMetadata(TT, props, names{k});
  end
end
%=============================================================================
function [defaultLabel, dataVariables] = parseArguments(args)
  % syncevents(TT, defaultLabel, 'EventDataVariables', vars): the default
  % label is the argument left when the name-value pairs are removed.
  defaultLabel = {};
  dataVariables = {};
  if mod(length(args), 2) == 1
    defaultLabel = args(1);
    args = args(2:end);
    value = defaultLabel{1};
    if ~(isscalar(value) || (ischar(value) && isrow(value)))
      error('Nelson:eventtable:NonScalarLabel', _('The default label supplied to syncevents must be a scalar.'));
    end
  end
  for k = 1:2:length(args)
    name = args{k};
    if ~((ischar(name) && isrow(name)) || (isstring(name) && isscalar(name))) || ~strcmpi(name, 'EventDataVariables')
      if isstring(name) || ischar(name)
        error('Nelson:eventtable:invalidArgumentName', _('Invalid argument name ''%s''. Use ''EventDataVariables''.'), char(name));
      end
      error('Nelson:eventtable:invalidArgumentName', _('Invalid argument name. Use ''EventDataVariables''.'));
    end
    dataVariables = args(k + 1);
  end
end
%=============================================================================
function column = eventColumn(values, eventRows)
  % The event values of the matched rows, missing values elsewhere.
  column = timetableMissingRows(values, numel(eventRows));
  matched = eventRows > 0;
  column(matched, :) = values(eventRows(matched), :);
end
%=============================================================================
function value = labelValue(label, labels)
  % Default label converted to the type of the event labels.
  if iscell(labels)
    value = {char(string(label))};
  elseif isstring(labels)
    value = string(label);
  else
    value = label;
  end
end
%=============================================================================
function TT = copyVariableMetadata(TT, eventProps, eventName)
  % The unit and the description of the event variable (the last one of
  % TT) follow it; the other variables keep theirs.
  index = find(strcmp(eventName, eventProps.VariableNames), 1);
  fields = {'VariableUnits', 'VariableDescriptions'};
  props = TT.Properties;
  n = numel(props.VariableNames);
  for k = 1:length(fields)
    source = eventProps.(fields{k});
    target = props.(fields{k});
    value = '';
    if ~isempty(source)
      value = source{index};
    end
    if isempty(target) && isempty(value)
      continue
    end
    target(end + 1:n) = {''};
    target{n} = value;
    props.(fields{k}) = target;
  end
  TT = subsasgn(TT, struct('type', '.', 'subs', 'Properties'), props);
end
%=============================================================================
