%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Negates a zpk model by negating its gain.
function sys = uminus(sysIn)
  sys = sysIn;
  sys.K = -sysIn.K;
end
%=============================================================================
