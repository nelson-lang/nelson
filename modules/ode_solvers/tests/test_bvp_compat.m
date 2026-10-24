%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = bvpset('RelTol', 1e-5, 'AbsTol', 1e-7, 'NMax', 200);
asserts.istrue(isstruct(options));
asserts.isequal(bvpget(options, 'RelTol'), 1e-5);
asserts.isequal(bvpget(options, 'FJacobian', 7), 7);
%=============================================================================
linearInit = bvpinit(linspace(0, 1, 5), [0; 1]);
linearSol = bvp4c(@(x, y)[y(2); 0], @(ya, yb)[ya(1); yb(1) - 1], ...
  linearInit, options);
asserts.isequal(linearSol.solver, 'bvp4c');
asserts.isequal(linearSol.stats.nmeshpoints, length(linearSol.x));
asserts.istrue(isfield(linearSol.stats, 'maxDefect'));
asserts.istrue(isfield(linearSol.stats, 'rmsDefect'));
asserts.istrue(isfield(linearSol.stats, 'residualRms'));
asserts.istrue(max(abs(linearSol.y(1, :) - linearSol.x)) < 1e-4);
[linearY, linearYp] = deval(linearSol, [0.25 0.75]);
asserts.istrue(max(abs(linearY(1, :) - [0.25 0.75])) < 1e-4);
asserts.istrue(max(abs(linearYp(1, :) - 1)) < 1e-4);
%=============================================================================
analyticOptions = bvpset(options, 'FJacobian', @bvpTestLinearJacobian, ...
  'BCJacobian', @bvpTestLinearBcJacobian, 'SingularTerm', zeros(2));
analyticSol = bvp4c(@(x, y)[y(2); 0], @(ya, yb)[ya(1); yb(1) - 1], ...
  linearInit, analyticOptions);
asserts.istrue(max(abs(analyticSol.y(1, :) - analyticSol.x)) < 1e-4);
asserts.istrue(analyticSol.stats.residualNorm < 1e-5);
%=============================================================================
clear global BVP_VECTORIZED_CALL_SEEN;
global BVP_VECTORIZED_CALL_SEEN;
BVP_VECTORIZED_CALL_SEEN = false;
vectorizedOptions = bvpset(options, 'Vectorized', 'on');
vectorizedSol = bvp4c(@bvpTestVectorizedRhs, @(ya, yb)[ya(1); yb(1) - 1], ...
  linearInit, vectorizedOptions);
asserts.istrue(max(abs(vectorizedSol.y(1, :) - vectorizedSol.x)) < 1e-4);
asserts.istrue(BVP_VECTORIZED_CALL_SEEN);
%=============================================================================
paramInit = bvpinit(linspace(0, 1, 6), 1, 0.5);
paramSol = bvp4c(@(x, y, p) p(1) * y, @(ya, yb, p)[ya(1) - 1; yb(1) - 2], ...
  paramInit, options);
asserts.istrue(abs(paramSol.parameters(1) - log(2)) < 2e-3);
asserts.istrue(abs(paramSol.y(1, length(paramSol.x)) - 2) < 1e-5);
capabilities = __ode_sundials_capabilities__();
if isfield(capabilities, 'kinsol') && capabilities.kinsol
  asserts.istrue(paramSol.stats.nfevals > 0);
end
%=============================================================================
extended = bvpxtend(linearSol, linspace(0, 1, 9));
asserts.isequal(length(extended.x), 9);
asserts.istrue(max(abs(extended.y(1, :) - extended.x)) < 1e-4);
%=============================================================================
bratuInit = bvpinit(linspace(0, 1, 7), [0; 0]);
bratuSol = bvp5c(@(x, y)[y(2); -exp(y(1))], @(ya, yb)[ya(1); yb(1)], ...
  bratuInit, options);
asserts.isequal(bratuSol.solver, 'bvp5c');
asserts.isequal(bratuSol.stats.nmeshpoints, length(bratuSol.x));
asserts.istrue(bratuSol.stats.nrefinements >= 0);
asserts.istrue(isfinite(bratuSol.stats.maxDefect));
asserts.istrue(abs(bratuSol.y(1, 1)) < 1e-5);
asserts.istrue(abs(bratuSol.y(1, length(bratuSol.x))) < 1e-5);
bratuCenter = deval(bratuSol, 0.5);
asserts.istrue(abs(bratuCenter(1) - 0.14053921090765503) < 5e-4);
%=============================================================================
