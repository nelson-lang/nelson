%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Light per-poll value refresh for the property inspector: display strings only
% (no kinds, no choices), so the inspector stays in sync with changes made by a
% script or by mouse/keyboard interaction without rebuilding the full model on
% every idle refresh. Returns a struct mapping each visible property name to its
% current display string.
%=============================================================================
function values = __graphics_inspector_values__(object)
  names = properties(object);
  raw = get(object);
  values = struct();
  for k = 1:numel(names)
    name = names{k};
    values.(name) = __graphics_value_string__(raw.(name));
  end
end
%=============================================================================
