%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_properties(f, ax, request)
  object = ax;
  if isfield(request, 'objectHandle')
    object = __web_graphics_handle__(request.objectHandle, 'object');
    if ~isgraphics(object) || ~local_belongs(object, f)
      error('Nelson:graphics:invalidTarget', 'Object belongs to another figure.');
    end
  elseif isfield(request, 'target') && strcmp(request.target, 'figure')
    object = f;
  elseif isfield(request, 'target') && strcmp(request.target, 'title')
    object = get(ax, 'Title');
  end
  writableNames = fieldnames(set(object));
  names = fieldnames(request.properties);
  args = {};
  for k = 1:numel(names)
    name = names{k};
    if ~isprop(object, name)
      error('Nelson:graphics:invalidProperty', 'This property cannot be edited here.');
    end
    if ~any(strcmp(name, writableNames))
      error('Nelson:graphics:readOnlyProperty', 'This property is read-only.');
    end
    args = [args, {name, request.properties.(name)}];
  end
  if ~isempty(args)
    if isequal(object, ax) && ~all(ismember(names, {'XGrid', 'YGrid', 'ZGrid'}))
      web_figure_reference_view(ax, 'capture');
    end
    set(object, args{:});
  end
end
%=============================================================================
function tf = local_belongs(object, f)
  if isequal(object, f)
    tf = true;
    return;
  end
  tf = isequal(ancestor(object, 'figure'), f);
end
%=============================================================================
