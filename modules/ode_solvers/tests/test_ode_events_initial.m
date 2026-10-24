%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
for direction = [-1 0 1]
  options = odeset('Events', @(t, y) odeTestInitialEvent(t, y, direction), ...
    'InitialStep', 0.1, 'MaxStep', 0.1);
  [t, y, te, ye, ie] = ode45(@(t, y) 1, [0 1], 0, options);
  asserts.istrue(abs(t(length(t)) - 1) < 1e-12);
  asserts.istrue(abs(y(length(y)) - 1) < 1e-10);
  if direction < 0
    asserts.istrue(isempty(te));
    asserts.istrue(isempty(ye));
    asserts.istrue(isempty(ie));
  else
    asserts.isequal(length(te), 1);
    asserts.istrue(abs(te(1)) < 1e-12);
    asserts.istrue(abs(ye(1)) < 1e-12);
    asserts.isequal(ie(1), 1);
  end
end
%=============================================================================
