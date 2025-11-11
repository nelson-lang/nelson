%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = std(varargin)
    if nargout <= 1
        varargout{1} = sqrt(var(varargin{:}));
    else
        [v, m] = var(varargin{:});
        y = sqrt(v);
        varargout{1} = y;
        varargout{2} = m;
    end
end
%=============================================================================