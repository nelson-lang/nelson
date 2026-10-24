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
    error('Nelson:deconv:vectorExpected', 'B and A must be vectors.');
  end

  if ((size(b, 1) && size(a, 2)) || (size(b, 2) && size(a, 1)))
    a = a.';
  end
  if a(1) == 0
    error('Nelson:deconv:leadingCoefficientZero', 'First coefficient of A must be nonzero.');
  end
  if isscalar(a)
    q = b / a;
    varargout{1} = q;
    varargout{2} = zeros(size(b), class(q));
    return;
  end

  [mb, nb] = size(b);
  nb = max(mb, nb);
  na = length(a);

  % result is single if B or A is single
  if issingle(b) || issingle(a)
    outclass = 'single';
  else
    outclass = 'double';
  end

  if (na > nb) || all(a == 0)
    q = zeros(1, 1, outclass);
    varargout{1} = q;
    varargout{2} = cast(b, class(q));
    return;
  end

  if nb - na + 1 <= 0
    q = zeros(1, 1, outclass);
  else
    q = zeros(1, nb - na + 1, outclass);
  end
  r = cast(b, outclass);

  for i = 1:nb - na + 1
    if (r(i) ~= 0)
      q(i) = r(i) / a(1);
      for j = 1:na
        r(i + j - 1) = r(i + j - 1) - q(i) * a(j);
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
