%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = bitget(varargin)
  % device floats hold double values, so the host computes them as double
  C = hostDoubleCall('bitget', varargin{:});
end
%=============================================================================
