%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sos = zp2sos(varargin)
  narginchk(3, 3);
  nargoutchk(0, 1);
  z = varargin{1};
  p = varargin{2};
  k = varargin{3};
  z = z(:);
  p = p(:);
  sectionCount = max(1, ceil(max(length(z), length(p)) / 2));
  sos = zeros(sectionCount, 6);
  gain = prod(k(:));
  for section = 1:sectionCount
    zi = signalTakePair(z, section);
    pi = signalTakePair(p, section);
    b = real(poly(zi));
    a = real(poly(pi));
    b = [zeros(1, 3 - length(b)), b];
    a = [zeros(1, 3 - length(a)), a];
    if section == 1
      b = b * gain;
    end
    sos(section, :) = [b, a];
  end
end
%=============================================================================
