%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
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
function varargout = ind2sub (dims, ind)
  narginchk(2, 2);
  if (isvector (dims) && all (round (dims) == dims))
    if (isnumeric (ind) && all (round (ind) == ind))
      ntot = prod (dims);
      if (all (ind > 0 & ind <= ntot))
        nd = length (dims);
        if (nargout > 0)
          vlen = nargout;
        else
          vlen = 1;
        end
        if (nd > vlen);
          dims(vlen) = prod (dims(vlen:nd));
          dims(vlen+1:nd) = [];
        end
        nd = length (dims);
        scale = [1, vector_cumprod(dims(:))];
        for i = nd:-1:2
          k = (ind >= scale(i));
          r = ones (size (ind));
          t = zeros (size (ind));
          t(k) = floor ((ind(k) - 1) / scale(i));
          r(k) = t(k) + 1;
          ind(k) = ind(k) - t(k) * scale(i);
          varargout{i} = r;
        end
        varargout{1} = ind;
        for i = nd+1:vlen
          varargout{i} = ones (size (ind));
        end
      else
        error(_('Index out of range'));
      end
    else
      error(_('Integer-valued index argument expected.'));
    end
  else
    error(_('dimensions must be an integer vector.'));
  end
end 
%=============================================================================
function r = vector_cumprod(x)
  len = length(x);
  r = zeros(1, len);
  r(1) = x(1);
  for k = 2:len
    r(k) = r(k-1) * x(k);
  end
end
%=============================================================================
