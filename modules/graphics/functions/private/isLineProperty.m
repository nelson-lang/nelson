%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isLineProperty(name)
  % True when name is a line property, including an unambiguous abbreviation
  % such as 'LineW' for 'LineWidth'. A string that is also a valid LineSpec
  % (for example 'm' for magenta or 'v' for a marker) stays positional and is
  % not treated as the Marker or Visible property.
  if ~(ischar(name) || isStringScalar(name))
    tf = false;
    return
  end
  if isValidGraphicsProperty('line', name)
    tf = true;
    return
  end
  if isempty(canonicalGraphicsProperty('line', name))
    tf = false;
    return
  end
  [~, ~, ~, msg] = colstyle(name, 'plot', false);
  tf = ~isempty(msg);
end
%=============================================================================
