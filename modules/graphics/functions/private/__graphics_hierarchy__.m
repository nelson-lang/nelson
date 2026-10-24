%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Builds a lightweight object hierarchy (handle, type, label, children) for the
% property inspector object browser. It carries no property values, only the
% names needed to draw the tree, and reads just the properties it needs with
% targeted get() calls so it never materialises bulk data (XData/ZData/CData).
%=============================================================================
function tree = __graphics_hierarchy__(figure)
  tree = local_node(figure);
end
%=============================================================================
function node = local_node(object)
  node = struct('handle', local_handle(object), ...
    'type', local_type(object), 'label', local_label(object), 'children', {{}});
  kids = {};
  if isprop(object, 'Children')
    children = get(object, 'Children');
    for k = 1:numel(children)
      child = children(k);
      if isgraphics(child)
        kids{end + 1} = local_node(child);
      end
    end
  end
  % Title and axis labels are handle-valued properties, not Children, but the
  % object browser lists them under the axes just like the reference inspector.
  decorators = {'Title', 'Subtitle', 'XLabel', 'YLabel', 'ZLabel'};
  for d = 1:numel(decorators)
    name = decorators{d};
    if isprop(object, name)
      handle = get(object, name);
      if numel(handle) == 1 && isgraphics(handle)
        kids{end + 1} = local_node(handle);
      end
    end
  end
  node.children = kids;
end
%=============================================================================
function text = local_type(object)
  if isprop(object, 'Type')
    text = char(get(object, 'Type'));
  else
    text = 'graphics_object';
  end
end
%=============================================================================
function text = local_label(object)
  text = '';
  if isprop(object, 'DisplayName')
    value = get(object, 'DisplayName');
    if ~isempty(value)
      text = char(value);
      return;
    end
  end
  if isprop(object, 'String')
    value = get(object, 'String');
    if ischar(value) && ~isempty(value)
      text = char(value);
      return;
    end
  end
  if isprop(object, 'Tag')
    value = get(object, 'Tag');
    if ~isempty(value)
      text = char(value);
    end
  end
end
%=============================================================================
function h = local_handle(object)
  try
    h = __web_graphics_handle__(object, 'number');
  catch
    h = 0;
  end
end
%=============================================================================
