%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(expm1(0), 0);
asserts.isequal(reallog(1), 0);
asserts.isequal(realsqrt(4), 2);
asserts.checkerror('reallog(-1)', sprintf(_('%s must be real.'), 'Result'), 'Nelson:validators:mustBeReal');
asserts.checkerror('realsqrt(-1)', sprintf(_('%s must be real.'), 'Result'), 'Nelson:validators:mustBeReal');
%=============================================================================
