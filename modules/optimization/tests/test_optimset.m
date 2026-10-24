%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
opts = optimset('TolX', 1e-8, 'MaxIter', 12);
asserts.isequal(opts.TolX, 1e-8);
asserts.isequal(opts.MaxIter, 12);
opts = optimset('FunctionTolerance', 1e-9, 'StepTolerance', 1e-8);
asserts.isequal(opts.TolFun, 1e-9);
asserts.isequal(opts.TolX, 1e-8);
%=============================================================================
