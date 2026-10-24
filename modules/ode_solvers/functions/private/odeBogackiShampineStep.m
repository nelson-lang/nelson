%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y3, y2, evals] = odeBogackiShampineStep(problem, t, y, h)
  k1 = odeRhs(problem, t, y);
  k2 = odeRhs(problem, t + h / 2, y + h * k1 / 2);
  k3 = odeRhs(problem, t + 3 * h / 4, y + 3 * h * k2 / 4);
  y3 = y + h * (2 * k1 / 9 + k2 / 3 + 4 * k3 / 9);
  k4 = odeRhs(problem, t + h, y3);
  y2 = y + h * (7 * k1 / 24 + k2 / 4 + k3 / 3 + k4 / 8);
  evals = 4;
end
%=============================================================================
