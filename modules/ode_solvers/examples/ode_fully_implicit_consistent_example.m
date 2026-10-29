%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Fully implicit residual example with ode15i.
%
% The residual equation is
%
%     F(t, y, yp) = yp + lambda * y = 0
%
% with exact solution y(t) = y0 * exp(-lambda * t). The initial value and
% initial slope are already consistent, so no separate initialization step is
% needed for this compact example.
%=============================================================================

decayRate = 2.5;
initialValue = 1.0;
initialSlope = -decayRate * initialValue;
finalTime = 1.0;

options = odeset('RelTol', 1e-8, 'AbsTol', 1e-10);
solution = ode15i(...
  @(t, y, yp) implicitDecayResidual(t, y, yp, decayRate), ...
  [0 finalTime], ...
  initialValue, ...
  initialSlope, ...
  options);

computedFinal = solution.y(1, length(solution.x));
expectedFinal = initialValue * exp(-decayRate * finalTime);
finalError = abs(computedFinal - expectedFinal);

[~, finalSlope] = deval(solution, finalTime);
residualAtFinal = implicitDecayResidual(finalTime, computedFinal, finalSlope, decayRate);

disp('Fully implicit residual problem');
disp(['  final value:    ', num2str(computedFinal)]);
disp(['  exact value:    ', num2str(expectedFinal)]);
disp(['  final error:    ', num2str(finalError)]);
disp(['  residual norm:  ', num2str(norm(residualAtFinal))]);

if finalError >= 1e-5
  error('Final value is outside tolerance.');
end
if norm(residualAtFinal) >= 1e-5
  error('Final residual is outside tolerance.');
end

%=============================================================================
% LOCAL FUNCTIONS
%=============================================================================

function residual = implicitDecayResidual(t, y, yp, lambda)
  residual = yp + lambda * y;
end
%=============================================================================
