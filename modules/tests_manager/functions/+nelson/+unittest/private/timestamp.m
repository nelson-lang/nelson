%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function t = timestamp()
  n = now();
  v = datevec(n);
  t = sprintf('%d-%02d-%02dT%02d:%02d:%02d', v(1), v(2), v(3), v(4), v(5), v(6));
end
