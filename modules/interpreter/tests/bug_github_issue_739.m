%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/739
% <-- Short Description -->
% special case with empty cell.
%=============================================================================
p = {};
p{:}
asserts.checkerror('R = eval(''p{:}'');', _('Empty expression!'));
R = evalc('p{:}');
asserts.isequal(R, '');
asserts.checkerror('k = p{:}', _('Empty expression!'));
%=============================================================================
P = [];
asserts.isequal(P(:), zeros(0, 1));
R = P(:);
asserts.isequal(R, zeros(0, 1));
%=============================================================================
c = {'f', 1};
c{2}
R = evalc('c{2}');
REF = '
ans =

     1

';
asserts.isequal(R, REF);
asserts.checkerror('k = p{:}', _('Empty expression!'));
%=============================================================================
