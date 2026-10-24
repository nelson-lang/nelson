%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pixels = __nelsonc_editor_icon__(name)
  persistent cached;
  key = strrep(strrep(name, '.svg', ''), '-', '_');
  if isempty(cached); cached = struct(); end
  if ~isfield(cached, key)
    filename = fullfile(modulepath('compiler'), 'resources', 'editor', strrep(name, '.svg', '.png'));
    [rgb, map, alpha] = imread(filename);
    a = repmat(double(alpha) / 255, [1, 1, 3]);
    cached.(key) = double(rgb) / 255 .* a + (1 - a) * 0.96078431372549;
  end
  pixels = cached.(key);
end
