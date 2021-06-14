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
function C = cov(varargin)
    % C = cov(A)
    % C = cov(A, w)
    % C = cov(A, B)
    % C = cov(A, B, w)
    if nargin < 1 || nargin > 3
        error(_('Wrong number of input arguments.'));
    end
    w = 0;
    if nargin == 1
        A = varargin{1};
        if isempty(A)
            C = NaN;
        elseif isvector(A)
            nobs = length(A);
            r = 1 / (nobs - 1);
            C = r * (norm(A)^2 - nobs * mean(A)^2);
        else
            nobs = size(A, 'r');
            m = nobs;
            if nobs > 1
                nobs = nobs - 1;
            end
            ac = A - sum(A, 1) ./ m;
            C = (ac' * ac) ./ nobs;
            if isscalar(C)
                C = real(C);
            end
        end
        return
    end
    if nargin == 2
        A = varargin{1};
        wOrB = varargin{2};
        if isscalar(A) && isscalar(wOrB)
            if wOrB == 0 || wOrB == 1
                C = 0;
            else
                C = zeros(2, 2);
            end
            return
        else
            B = wOrB;
        end
    end
    if nargin == 3
        A = varargin{1};
        B = varargin{2};
        w = varargin{3};
    end

    if ~(w == 0 || w == 1)
        error(_('Wrong value for #3 argument: 0 or 1 expected.'));
    end

    A = A(:);
    B = B(:);
    if (length(A) ~= length(B))
      error(_('arguments #1 and #2 must have same length.'));
    end
    nrmlztn = 0;
    nobs = length(A);
    r = 1 / (nobs - 1 + nrmlztn);
    mA = mean(A);
    mB = mean(B);
    C = r * [norm(A) ^ 2 - nobs * mA ^ 2, A' * B - nobs * mA * mB; 0, norm(B)^2 - nobs * mB ^ 2];
    C(2, 1) = C(1, 2);
end
%=============================================================================
