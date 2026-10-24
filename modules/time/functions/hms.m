%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, m, s] = hms(t)
  narginchk(1, 1);
  nargoutchk(0, 3);
  if isa(t, 'duration')
    total = double(t);
    signValue = sign(total);
    total = abs(total);
    h = floor(total ./ 3600) .* signValue;
    r = total - floor(total ./ 3600) .* 3600;
    m = floor(r ./ 60);
    s = r - m .* 60;
  else
    v = datevec(datenum(t));
    h = reshape(v(:, 4), size(datenum(t)));
    m = reshape(v(:, 5), size(datenum(t)));
    s = reshape(v(:, 6), size(datenum(t)));
  end
end
%=============================================================================
