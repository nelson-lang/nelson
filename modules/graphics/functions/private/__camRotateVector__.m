%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rotate the 3-vector v about axis by angleDeg degrees (Rodrigues formula).
% Used by the cam* orbit/pan/roll commands. Returns a row vector.
function r = __camRotateVector__(v, axis, angleDeg)
  v = double(v(:).');
  axis = double(axis(:).');
  n = norm(axis);
  if (n == 0 || ~isfinite(n))
    r = v;
    return
  end
  k = axis / n;
  th = angleDeg * pi / 180;
  r = v * cos(th) + cross(k, v) * sin(th) + k * (dot(k, v)) * (1 - cos(th));
end
%=============================================================================
