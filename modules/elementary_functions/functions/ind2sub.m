%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
        scale = [1, cumprod(dims(:))'];
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
