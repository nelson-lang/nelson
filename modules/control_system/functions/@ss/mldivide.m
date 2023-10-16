%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mldivide(varargin)
    narginchk(2, 2),
    nargoutchk(0, 1);
    sysA = ss(varargin{1});
    sysB = ss(varargin{2});
    sys = inv(sysA) * sysB;
    varargout{1} = sys;
end
%=============================================================================
