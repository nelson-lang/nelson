%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(obj, varargin)
  % Shared by table, timetable and eventtable: the header names the class,
  % after the size unless the table is 1x1, and flags a table without rows
  % or without variables as empty.
  if nargin == 2
    name = varargin{1};
  else
    name = inputname(1);
  end
  currentFormat = format();
  isLoose = strcmp(currentFormat.LineSpacing, 'loose');
  if ~isempty(name)
    if isLoose
      fprintf('\n');
    end
    builtin('disp', [name, ' =']);
  end
  if isLoose
    fprintf('\n');
  end
  nRows = height(obj);
  nVars = width(obj);
  if nRows == 0 || nVars == 0
    header = sprintf('  %d%s%d empty %s', nRows, char(215), nVars, class(obj));
  elseif nRows == 1 && nVars == 1
    header = ['  ', class(obj)];
  else
    header = sprintf('  %d%s%d %s', nRows, char(215), nVars, class(obj));
  end
  header = [header, eventsText(obj)];
  builtin('disp', header);
  if isLoose
    fprintf('\n');
  end
  displayPreamble(obj, isLoose);
  disp(obj);
end
%=============================================================================
function text = eventsText(obj)
  % A timetable with a nonempty event table names its number of events.
  text = '';
  if ~strcmp(class(obj), 'timetable')
    return
  end
  props = subsref(obj, struct('type', '.', 'subs', 'Properties'));
  if ~isa(props.Events, 'eventtable')
    return
  end
  n = height(props.Events);
  if n == 1
    text = ' with 1 event';
  elseif n > 1
    text = sprintf(' with %d events', n);
  end
end
%=============================================================================
