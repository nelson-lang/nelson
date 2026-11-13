%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function resetAxesTextObjects(ax)
  % Deletes title, subtitle and labels; the axes recreates them with default values.
  names = {'Title', 'Subtitle', 'XLabel', 'YLabel', 'ZLabel'};
  for k = 1:numel(names)
    if isprop(ax, names{k})
      h = get(ax, names{k});
      if ~isempty(h) && isvalid(h)
        delete(h);
      end
    end
  end
end
%=============================================================================
