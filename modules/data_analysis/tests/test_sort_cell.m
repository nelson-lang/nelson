%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = {'Smith','Burns'; 'Jones','Matthews'; 'Peterson','Adams'};
R = sort(A);
REF = {'Jones', 'Adams';
'Peterson', 'Burns';
'Smith', 'Matthews'};
assert_isequal(R, REF);
%=============================================================================
A = {'Smith','Burns'; 'Jones','Matthews'; 'Peterson','Adams'};
[R, I] = sort(A);
REF = [2     3;
3     1;
1     2];
assert_isequal(I, REF);
%=============================================================================
A = {'Smith';'Burns'; 'Jones';'Matthews'; 'Peterson';'Adams'};
R = sort(A);
REF = { 'Adams'; 'Burns'; 'Jones'; 'Matthews'; 'Peterson'; 'Smith'};
assert_isequal(R, REF);
%=============================================================================
A = {'Smith';'Burns'; 'Jones';'Matthews'; 'Peterson';'Adams'};
[R, I] = sort(A);
REF = [     6;     2;     3;     4;     5;     1];
assert_isequal(I, REF);
%=============================================================================
A = {'Smith','Burns', 'Jones','Matthews', 'Peterson','Adams'};
R = sort(A);
REF = {'Adams','Burns','Jones','Matthews','Peterson','Smith'};
assert_isequal(R, REF);
%=============================================================================
A = {'Smith','Burns', 'Jones','Matthews', 'Peterson','Adams'};
[R, I] = sort(A);
REF = [ 6     2     3     4     5     1];
assert_isequal(I, REF);
%=============================================================================
A = {'Smith','Burns', 'Jones','Matthews', 'Peterson','Adams'};
assert_checkerror('[R, I] = sort(A, 1)', _('Only one input parameter is supported for cell arrays.'));
%=============================================================================
