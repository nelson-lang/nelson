%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[C, ia, ic] = unique(single([10, 20, 20, 30, 20, 40]), 'rows');
C_REF = single([10 20 20 30 20 40]);
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
ic_REF = 1;
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(single([10, 20, 20, 30, 20, 40]), 'rows');
C_REF = single([10 20 20 30 20 40]);
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(single([10, 20, 20, 30, 20, 40]), 'rows');
C_REF = single([10 20 20 30 20 40]);
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(single([10, 20, 20, 30, 20, 40]));
C_REF = single([10 20 30 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
ic_REF = [1; 2; 2; 3; 2; 4];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(single([10, 20, 20, 30, 20, 40]));
C_REF = single([10 20 30 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(single([10, 20, 20, 30, 20, 40]));
C_REF = single([10 20 30 40]);
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(single([10, 20, 20, 30, 20, 40]'), 'rows');
C_REF = single([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
ic_REF = [1; 2; 2; 3; 2; 4];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(single([10, 20, 20, 30, 20, 40]'), 'rows');
C_REF = single([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(single([10, 20, 20, 30, 20, 40]'), 'rows');
C_REF = single([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
%=============================================================================


