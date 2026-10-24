%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __web_figure_deferred_action__(handle, encoded)
  request = jsondecode(encoded);
  try
    f = __web_graphics_handle__(handle, 'object');
    if ~isgraphics(f, 'figure')
      return;
    end
    for name = {'axesHandle', 'objectHandle'}
      if isfield(request, name{1}) && request.(name{1}) > 0
        target = __web_graphics_handle__(request.(name{1}), 'object');
        if strcmp(name{1}, 'axesHandle') && ~isgraphics(target, 'axes')
          return;
        end
        if ~isgraphics(target) || ~isequal(ancestor(target, 'figure'), f)
          return;
        end
      end
    end
  catch
    return;
  end
  __web_figure_action__(handle, encoded, false);
end
