%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = odephas3(t, y, flag, varargin)
  narginchk(3, Inf);
  status = odeOutputPlot('phase3', t, y, flag);
end
%=============================================================================
