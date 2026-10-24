%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function entries = __web_figure_list__()
  entries = {};
  figures = findall(0, 'Type', 'figure');
  for k = 1:numel(figures)
    f = figures(k);
    h = __web_graphics_handle__(f, 'number');
    name = get(f, 'Name');
    label = name;
    if strcmp(get(f, 'NumberTitle'), 'on')
      label = ['Figure ', num2str(get(f, 'Number'))];
      if ~isempty(name)
        label = [label, ': ', name];
      end
    end
    entries{end + 1} = struct('handle', h, 'title', label, ...
      'windowStyle', get(f, 'WindowStyle'), 'visible', char(get(f, 'Visible')));
  end
end
