%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = odeComplexImplicitEvent(t, y, yp)
  asserts.isfalse(isreal(y));
  asserts.isfalse(isreal(yp));
  value = real(y) - exp(-0.25);
end
%=============================================================================
