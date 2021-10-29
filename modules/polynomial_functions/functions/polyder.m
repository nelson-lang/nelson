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
function [a, b] = polyder(varargin)
  narginchk(1, 2)
  nargoutchk(0, 2)
  u = varargin{1};
  if nargin < 2
    v = 1;
  else
    v = varargin{2};
  end
  v = v(:).';
  u = u(:).';
  nu = length(u);
  nv = length(v);
  if nu >= 2
    up = u(1:nu-1) .* (nu-1:-1:1);
  else
    up = 0;
  end

  if nv >= 2
    vp = v(1:nv-1) .* (nv-1:-1:1);
  else
    vp = 0;
  end

  av = conv(up, v);
  au = conv(u, vp);

  i = length(av);
  j = length(au);

  z = zeros(1, abs(i - j));
  if i > j
    au = [z, au];
  elseif i < j
    av = [z, av];
  end

  if nargout < 2
    a = av + au;
  else
    a = av - au;
  end

  a = polyder_trim_a(a, u, v);
  b = polyder_trim_b(v);

  if length(a) > max(nu + nv - 2,1)
    a = a(2:end);
  end

end
%=============================================================================
function c = superiorclass(a, b)
  if (isdouble(a) && issingle(b))
    c = 'double';
  elseif (issingle(a) && isdouble(b))
    c = 'double';
  else
    c = 'double';
  end
end
%=============================================================================
function a = polyder_trim_a(a, u, v)
   f = find(a ~= 0);
   if isempty(f)
     a = zeros(superiorclass(u, v));
   else 
     a = a(f(1):end);
   end
end
%=============================================================================
function b = polyder_trim_b(v)
  b = conv(v, v);
  f = find(b ~= 0);
  if isempty(f)
    b = zeros(class(v));
  else
    b = b(f(1):end);
  end
end
%=============================================================================
