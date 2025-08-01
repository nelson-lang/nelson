%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = ss([-.1 -1;1 0],[1;0],[0 1],0);
wc = gram(sys, 'c');
REF = [5 0; 0 5];
assert_isapprox(wc, REF, 1e-3);
%=============================================================================
wc = gram(sys, 'o');
REF = [5.0000    0.5000; 0.5000    5.0500];
assert_isapprox(wc, REF, 1e-3);
%=============================================================================
A = [-1 0 0; 1/2 1 0; 1/2 0 -1] / 2;
B = [1 0; 0 -1; 0 1];
C = [0 0 1; 1 1 0];
D = zeros(size(C, 1), size(B, 2));
sys = ss(A,B, C, D, 2.4);
wc = gram(sys, 'c');
REF = [1.3333   -0.1333   -0.2222;
-0.1333    1.4000   -0.7244;
-0.2222   -0.7244    1.5185];
assert_isapprox(wc, REF, 1e-3);
%=============================================================================
wc = gram(sys, 'o');
REF = [  1.3185    0.9333   -0.2222;
0.9333    1.3333    0.0000;
-0.2222    0.0000    1.3333];
assert_isapprox(wc, REF, 1e-3);
%=============================================================================
assert_checkerror("gram(tf(1,1),'o')", _("Command cannot be used for models of class 'tf'."));
%=============================================================================
