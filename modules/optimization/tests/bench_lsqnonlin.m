%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
tic
opts = optimoptions('lsqnonlin', 'Jacobian', 'on', 'FunctionTolerance', 1e-10, 'StepTolerance', 1e-10);
[x, resnorm] = lsqnonlin(@optimTestResidualWithJacobian, [0; 0], [], [], opts);
toc
asserts.istrue(max(abs(x(:) - [4; -3])) < 1e-5);
asserts.istrue(abs(resnorm) < 1e-6);
%=============================================================================
