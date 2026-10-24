%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ET, TT2] = extractevents(TT, varargin)
  % Event table made of rows of TT, selected by row subscripts or by a
  % categorical vector of labels (one per row, <undefined> for no event).
  if nargin < 2
    error('Nelson:minrhs', _('Invalid argument list. Function requires %d more input(s).'), 2 - nargin);
  end
  [opts, preserve] = parseOptions(varargin(2:end));
  if nargout < 2 && ~isempty(preserve)
    error('Nelson:eventtable:ExtracteventsMissingSecondOutput', _('Using extractevents and specifying PreserveEventVariables without assigning the second output to a workspace variable does not modify the input timetable.'));
  end
  selector = varargin{1};
  if isa(selector, 'categorical')
    rows = categoricalRows(TT, selector, opts);
    opts.values.EventLabels = selector(rows);
  else
    rows = selector;
  end
  [names, options] = eventVariables(TT, opts);
  ET = eventtable(selectRows(TT, rows, names), options{:});
  if nargout > 1
    TT2 = TT;
    if isempty(preserve) || ~logical(preserve{1})
      TT2 = removevars(TT, names);
    end
  end
end
%=============================================================================
function [opts, preserve] = parseOptions(args)
  % preserve: {} or {PreserveEventVariables value}.
  optionNames = {'EventLabels', 'EventLabelsVariable', 'EventLengths', 'EventLengthsVariable', ...
    'EventEnds', 'EventEndsVariable', 'EventDataVariables', 'PreserveEventVariables'};
  opts = struct('values', struct(), 'names', struct());
  preserve = {};
  for k = 1:2:length(args)
    name = args{k};
    idx = [];
    if (ischar(name) && isrow(name)) || (isstring(name) && isscalar(name))
      idx = find(strcmpi(char(name), optionNames), 1);
    end
    if isempty(idx)
      error('Nelson:eventtable:invalidArgumentName', _('Invalid argument at position %d. Name must be one of these: %s.'), k + 2, strjoin(optionNames, ', '));
    end
    if k == length(args)
      error('Nelson:eventtable:nameValuePair', _('Invalid argument at position %d. Make sure name-value arguments include both the name and the value.'), k + 2);
    end
    name = optionNames{idx};
    if strcmp(name, 'PreserveEventVariables')
      preserve = args(k + 1);
    elseif endsWith(name, 'Variable') || endsWith(name, 'Variables')
      opts.names.(name) = args{k + 1};
    else
      opts.values.(name) = args{k + 1};
    end
  end
  if ~isempty(preserve) && isempty(fieldnames(opts.names))
    error('Nelson:eventtable:ExtracteventsMissingEventVariables', _('In order to specify PreserveEventVariables, provide at least one of EventDataVariables, EventLabelsVariable, EventLengthsVariable, or EventEndsVariable.'));
  end
end
%=============================================================================
function rows = categoricalRows(TT, labels, opts)
  % The rows labelled by a categorical vector: its defined elements.
  if isfield(opts.values, 'EventLabels') || isfield(opts.names, 'EventLabelsVariable')
    error('Nelson:eventtable:CategoricalAndLabels', _('Event labels or an event labels variable are not supported when the second input is a categorical.'));
  end
  if ~isvector(labels) || numel(labels) ~= height(TT)
    error('Nelson:eventtable:InvalidLabelsCategorical', _('The number of elements in the labels categorical vector must be the same as the number of rows of the input timetable.'));
  end
  rows = find(~isundefined(labels(:)));
end
%=============================================================================
function [names, options] = eventVariables(TT, opts)
  % Variables of TT copied to the event table, event lengths or ends
  % first, then labels, then data, and the eventtable options naming them.
  fields = {'EventLengthsVariable', 'EventEndsVariable', 'EventLabelsVariable'};
  names = {};
  options = {};
  for k = 1:length(fields)
    if isfield(opts.names, fields{k})
      name = timetableVariableNames(TT, opts.names.(fields{k}));
      names = [names, name];
      options = [options, {fields{k}, name{1}}];
    end
  end
  if isfield(opts.names, 'EventDataVariables')
    dataNames = timetableVariableNames(TT, opts.names.EventDataVariables);
    for k = 2:2:length(options)
      if any(strcmp(options{k}, dataNames))
        error('Nelson:eventtable:ConflictingEventAndDataVars', _('%s must not be in the list of EventDataVariables.'), options{k - 1});
      end
    end
    names = [names, dataNames];
  end
  valueFields = fieldnames(opts.values);
  for k = 1:length(valueFields)
    options = [options, {valueFields{k}, opts.values.(valueFields{k})}];
  end
end
%=============================================================================
function sub = selectRows(TT, rows, names)
  % The event rows of TT with the event variables only.
  if ischar(rows) && ~strcmp(rows, ':') || isstring(rows) || iscell(rows)
    invalidRows();
  end
  try
    sub = subsref(TT, struct('type', '()', 'subs', {{rows, names}}));
  catch
    invalidRows();
  end
end
%=============================================================================
function invalidRows()
  error('Nelson:eventtable:MustBeCategoricalOrTime', _('The second input must be a valid timetable row subscript or a categorical vector.'));
end
%=============================================================================
