%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function texts = compactElementText(obj)
  % Element texts shown when a calendarDuration row is displayed inside a cell or a struct.
  texts = reshape(string(obj), 1, []);
end
%=============================================================================
