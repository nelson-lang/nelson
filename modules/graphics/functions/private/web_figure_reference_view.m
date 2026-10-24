%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_reference_view(ax, action)
  reference = __web_figure_reference__(ax, strcmp(action, 'reference'));
  if ~strcmp(action, 'restore')
    return;
  end
  % Restore orientation first, then values and finally their saved auto/manual modes.
  set(ax, 'View', reference.View);
  names = fieldnames(reference);
  for modes = [false true]
    for k = 1:numel(names)
      name = names{k};
      if ~strcmp(name, 'View') && endsWith(name, 'Mode') == modes
        set(ax, name, reference.(name));
      end
    end
  end
end
