%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = MException_extraction(exception, varargin)
    n = numel(varargin);
    switch (n)
     case 1
        name = varargin{1};
        s = struct(exception);
        r = s.(name);
    case 2
        s = struct(exception);
        name = varargin{1};
        rows = varargin{2};
        r = s.(name)(rows);
    case 3
        s = struct(exception);
        name = varargin{1};
        rows = varargin{2};
        cols = varargin{3};
        r = s.(name)(rows, cols);
    otherwise
        error('not managed.');
    end
end
%=============================================================================
