%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = deconv(varargin)
  
  narginchk(2, 2);
  nargoutchk(0, 2);
  b = varargin{1};
  a = varargin{2};
  
  if (~(isvector(a) && isvector(b)))
    error(_('B and A must be vectors.'));
  end
  
  if ((size(b, 1) && size(a, 2)) || (size(b, 2) && size(a, 1)))
    a = a.';
  end
  if a(1) == 0
    error(_('First coefficient of A must be non-zero.'));
  end
  if isscalar(a)
    varargout{1} = b / a;
    varargout{2} = zeros(size(b));
    return;
  end

  [mb, nb] = size(b);
  nb = max(mb, nb);
  na = length(a);
  
  if (na > nb) || all(a == 0)
    if issingle(b) && issingle(a)
      q = zeros(class(b));
    else
      q = zeros(class(2));
    end
    varargout{1} = q;
    varargout{2} = cast(b, class(q));
    return;
  end
  
  if nb - na + 1 <= 0
    q = zeros(1, 1);
  else
    q = zeros(1, nb - na + 1);
  end
  r = b;
  
  for i = 1:nb-na+1
    if (r(i) ~= 0)
      q(i) = r(i) / a(1);
      for j = 1:na
        r(i+j-1) = r(i+j-1) - q(i)*a(j);
      end
    end
  end
  if mb ~= 1
    q = q(:);
  end    
 
  varargout{1} = q;
  varargout{2} = r;
end
%=============================================================================
