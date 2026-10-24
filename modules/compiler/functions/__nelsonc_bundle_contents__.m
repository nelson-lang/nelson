%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Returns the 'Contents' directory of the installed macOS application bundle
% that hosts the given location, or '' when the location is not inside one.
% A bundle is recognized only when both 'PlugIns' and 'Resources' live under
% that 'Contents' directory.
function contents = __nelsonc_bundle_contents__(location)
  contents = '';
  if ~ismac() || isempty(location)
    return;
  end
  normalized = regexprep(strrep(location, '\', '/'), '/+$', '');
  segment = '/Contents';
  position = strfind(normalized, [segment, '/']);
  if ~isempty(position)
    candidate = normalized(1:position(end) + numel(segment) - 1);
  elseif endsWith(normalized, segment)
    candidate = normalized;
  else
    return;
  end
  if isfolder(fullfile(candidate, 'PlugIns')) && isfolder(fullfile(candidate, 'Resources'))
    contents = candidate;
  end
end
