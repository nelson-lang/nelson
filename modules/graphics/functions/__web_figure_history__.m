%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = __web_figure_history__(f, operation, before, group)
  history = __web_figure_history_data__(f);
  if isempty(history)
    history = struct('undo', {{}}, 'redo', {{}});
  end
  result = struct('canUndo', ~isempty(history.undo), 'canRedo', ~isempty(history.redo));
  switch operation
    case 'status'
      return;
    case 'begin'
      result = __web_figure_state__(__web_graphics_handle__(f, 'number'));
      result = rmfield(result, {'canUndo', 'canRedo', 'currentAxes'});
      return;
    case 'commit'
      after = __web_figure_history__(f, 'begin');
      if isequal(before, after)
        return;
      end
      entry = struct('before', before, 'after', after, 'group', group);
      if ~isempty(group) && ~isempty(history.undo) && strcmp(history.undo{end}.group, group)
        entry.before = history.undo{end}.before;
        history.undo(end) = [];
      end
      history.undo{end + 1} = entry;
      history.undo = history.undo(max(1, end - 49):end);
      history.redo = {};
    case {'undo', 'redo'}
      if isempty(history.(operation))
        return;
      end
      entry = history.(operation){end};
      current = __web_figure_history__(f, 'begin');
      if ~isequal(topology(current), topology(entry.after))
        __web_figure_history_data__(f, []);
        error('Nelson:graphics:historyChanged', 'Figure objects changed; view history was cleared.');
      end
      if strcmp(operation, 'undo')
        desired = entry.before; previous = entry.after; opposite = 'redo';
      else
        desired = entry.after; previous = entry.before; opposite = 'undo';
      end
      try
        web_figure_restore_snapshot(f, desired, previous);
      catch failure
        web_figure_restore_snapshot(f, current, desired);
        rethrow(failure);
      end
      history.(operation)(end) = [];
      history.(opposite){end + 1} = entry;
    otherwise
      error('Nelson:graphics:invalidHistory', 'Unknown history operation.');
  end
  __web_figure_history_data__(f, history);
end

function handles = topology(state)
  handles = state.handle;
  for axis = state.axes
    handles(end + 1) = axis{1}.handle;
    for object = axis{1}.objects
      handles(end + 1) = object{1}.handle;
    end
  end
end
