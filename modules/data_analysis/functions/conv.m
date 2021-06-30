%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
function z = conv(a, b, shape)
    if nargin < 3
        shape = 'full';
    end    
    a_is_col = size(a, 1) > size(a, 2);
    b_is_col = size(b, 1) > size(b, 2);
    if (a_is_col || b_is_col)
        z = conv2(a(:), b(:), shape);
    else
        z = conv2(a(:).', b(:).', shape);
    end
    if (numel(a) <= numel(b))
        maxdims = min(find(size(b) == max(size(b))));
    else
        maxdims = min(find(size(a) == max(size(a))));
    end
    if (maxdims > 1)
        z = reshape(z, [ones(1, maxdims - 1), numel(z)]);
    end
end
%=============================================================================
