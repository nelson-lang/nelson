%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = graphicsIsPropertyNameToken(value, GOType)
  % True when VALUE names a property of GOType, given either in full or as an
  % unambiguous abbreviation (e.g. 'FaceC' -> 'FaceColor'). A colour
  % specification sitting in a positional colour slot is never read as a
  % property name, so fill(x, y, 'y') keeps 'y' as the colour even though it is
  % a unique leading substring of 'YData'. Wrappers use this to split positional
  % data from the name/value tail; the tail is then resolved where it is set.
  tf = false;
  if ~ischar(value) && ~(isstring(value) && isscalar(value))
    return
  end
  value = char(value);
  if any(strcmpi(value, getColorShortNameList())) || any(strcmpi(value, getColorNameList()))
    return
  end
  tf = ~isempty(canonicalGraphicsProperty(GOType, value));
end
%=============================================================================
