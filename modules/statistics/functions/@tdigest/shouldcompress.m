%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = shouldcompress(td)
  st = struct(td);
  % Use a slightly higher threshold to reduce compression frequency
  % This trades some memory for better performance
  threshold = max(st.compression * 1.5, st.compression + 50);
  tf = (numel(st.means) > threshold);
end
%=============================================================================
