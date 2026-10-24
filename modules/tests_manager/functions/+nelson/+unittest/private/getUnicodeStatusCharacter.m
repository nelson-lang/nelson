%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = getUnicodeStatusCharacter(status)
  switch status %#ok<NLS0008>
    case 'Pass'
      r = '🟢';
    case 'Fail'
      r = '🔴';
    case {'Skip', 'Interactive', 'No display'}
      r = '🟡';
    otherwise
      r = status;
  end
end
%=============================================================================
