%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_restore_snapshot(f, desired, previous)
  restore(f, struct('Name', desired.name, 'NumberTitle', desired.numberTitle, 'Color', desired.color), ...
    struct('Name', previous.name, 'NumberTitle', previous.numberTitle, 'Color', previous.color));
  for index = 1:numel(desired.axes)
    next = desired.axes{index}; old = previous.axes{index};
    ax = __web_graphics_handle__(next.handle, 'object');
    if ~strcmp(next.title, old.title)
      set(get(ax, 'Title'), 'String', next.title);
    end
    if next.projectionFloor ~= old.projectionFloor
      web_figure_floor(ax, next.projectionFloor);
    end
    restore(ax, rmfield(next, {'handle', 'title', 'objects', 'projectionFloor'}), ...
      rmfield(old, {'handle', 'title', 'objects', 'projectionFloor'}));
    for objectIndex = 1:numel(next.objects)
      object = next.objects{objectIndex}; oldObject = old.objects{objectIndex};
      target = __web_graphics_handle__(object.handle, 'object');
      object.Visible = object.visible; oldObject.Visible = oldObject.visible;
      restore(target, rmfield(object, {'handle', 'type', 'visible'}), ...
        rmfield(oldObject, {'handle', 'type', 'visible'}));
    end
  end
end

function restore(target, desired, previous)
  names = fieldnames(desired);
  changed = {};
  for index = 1:numel(names)
    name = names{index};
    if ~isequal(desired.(name), previous.(name))
      changed{end + 1} = name;
    end
  end
  if isempty(changed)
    return;
  end
  if any(strcmp(changed, 'View'))
    set(target, 'View', desired.View);
  end
  for modes = [false true]
    for index = 1:numel(names)
      name = names{index};
      if ~strcmp(name, 'View') && endsWith(name, 'Mode') == modes ...
          && (any(strcmp(name, changed)) || modes)
        set(target, name, desired.(name));
      end
    end
  end
end
