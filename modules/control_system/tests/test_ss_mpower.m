%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
A = [0 1; 0 0];
B = [0; 1];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
sys2 = sys ^ 3;
REF_A = [0 1 0 0 0 0;
  0 0 1 0 0 0;
  0 0 0 1 0 0;
  0 0 0 0 1 0;
  0 0 0 0 0 1;
  0 0 0 0 0 0];
REF_B = [0; 0; 0; 0; 0; 1];
REF_C = [1 0 0 0 0 0];
REF_D = 0;
asserts.isequal(sys2.A, REF_A)
asserts.isequal(sys2.B, REF_B)
asserts.isequal(sys2.C, REF_C)
asserts.isequal(sys2.D, REF_D)
%=============================================================================
A = [0 1; 0 0];
B = [0; 1];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
sys2 = sys ^ -3;

REF_A = [0 1 0 0 0 0 0;
  0 0 1 0 0 0 0;
  0 0 0 1 0 0 0;
  0 0 0 0 1 0 0;
  0 0 0 0 0 1 0;
  0 0 0 0 0 0 1;
  1 0 0 0 0 0 0];
REF_B = [0; 0; 0; 0; 0; 0; -1];
REF_C = [0 0 0 0 0 0 1]
REF_D = 0;
REF_E = [1 0 0 0 0 0 0;
  0 1 0 0 0 0 0;
  0 0 1 0 0 0 0;
  0 0 0 1 0 0 0;
  0 0 0 0 1 0 0;
  0 0 0 0 0 1 0;
  0 0 0 0 0 0 0];
asserts.isequal(sys2.A, REF_A)
asserts.isequal(sys2.B, REF_B)
asserts.isequal(sys2.C, REF_C)
asserts.isequal(sys2.D, REF_D)
asserts.isequal(sys2.E, REF_E)
%=============================================================================
