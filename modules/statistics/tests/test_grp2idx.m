%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('grp2idx'), 1);
asserts.isequal(nargout('grp2idx'), 3);
%=============================================================================
[g, gN, gL] = grp2idx({'b'; 'a'; 'b'; ''; 'c'});
asserts.istrue(isnan(g(4)));
asserts.isequal(g, [1; 2; 1; NaN; 3]);
asserts.isequal(g([1 2 3 5]), [1; 2; 1; 3]);
asserts.isequal(gN, {'b'; 'a'; 'c'});
asserts.isequal(gL, {'b'; 'a'; 'c'});
%=============================================================================
[g2, gN2, gL2] = grp2idx([3; 1; 3; NaN; 2]);
asserts.istrue(isnan(g2(4)));
asserts.isequal(g2([1 2 3 5]), [3; 1; 3; 2]);
asserts.isequal(gN2, {'1'; '2'; '3'});
asserts.isequal(gL2, [1; 2; 3]);
%=============================================================================
[g3, gN3, gL3] = grp2idx(logical([true false true]));
asserts.isequal(g3, [2 1 2]);
asserts.isequal(gN3, {'false'; 'true'});
asserts.isequal(gL3, [false; true]);
%=============================================================================
asserts.checkerror('grp2idx()', _('Not enough input arguments.'));
asserts.checkerror('grp2idx({1})', sprintf(_('%s must be a grouping variable.'), 'S'));
asserts.checkerror('grp2idx([1 2; 3 4])', sprintf(_('%s must be a vector or text array.'), 'S'));
%=============================================================================
