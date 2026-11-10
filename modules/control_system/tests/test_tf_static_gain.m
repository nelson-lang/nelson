%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Discrete static gain keeps its sample time
sys = tf(3, 1, 0.5);
asserts.istrue(isstatic(sys));
asserts.isequal(sys.Ts, 0.5);
asserts.isequal(sys.Variable, 'z');
%=============================================================================
sys = tf({3, 0}, {1, 1}, 0.5);
asserts.istrue(isstatic(sys));
asserts.isequal(sys.Variable, 'z');
%=============================================================================
% Zero numerator over a constant is static
sys = tf(0, 1);
asserts.istrue(isstatic(sys));
sys = tf({0, 0}, {1, 1});
asserts.istrue(isstatic(sys));
sys = tf(0, 1, 0.5);
asserts.istrue(isstatic(sys));
asserts.isequal(sys.Variable, 'z');
%=============================================================================
% A zero numerator over a polynomial is dynamic
sys = tf(0, [1 2]);
asserts.isfalse(isstatic(sys));
asserts.isequal(sys.Numerator, {[0 0]});
sys = tf({0, 1}, {[1 2], 1});
asserts.isfalse(isstatic(sys));
%=============================================================================
