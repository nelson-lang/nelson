%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function remove(filename, names)
  % Removes entries from a ZIP file without recompressing the others.
  narginchk(2, 2);
  __zip_rewrite__(char(filename), string(names), strings(0, 1), strings(0, 1));
end
%=============================================================================
