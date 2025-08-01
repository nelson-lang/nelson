%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/514
% <-- Short Description -->
% C{3}=4 should create a cell with good dimensions. 
%=============================================================================
clear('C');
C{3} = 4;
REF = {[] [] 4};
assert_isequal(C, REF);
%=============================================================================
clear('D');
D(4) = 3;
REF = [0 0 0 3];
assert_isequal(D, REF);
%=============================================================================
clear('E');
E(2, 2) = 5;
REF = [0 0; 0 5];
assert_isequal(E, REF);
%=============================================================================
