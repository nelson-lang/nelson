%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/739
% <-- Short Description -->
% special case with empty cell.
%=============================================================================
p = {};
p{:}
assert_checkerror('R = eval(''p{:}'');', _('Empty expression!'));
R = evalc('p{:}');
assert_isequal(R, '');
assert_checkerror('k = p{:}', _('Empty expression!'));
%=============================================================================
P = [];
assert_isequal(P(:), zeros(0, 1));
R = P(:);
assert_isequal(R, zeros(0, 1));
%=============================================================================
c = {'f', 1};
c{2}
R = evalc('c{2}');
REF = '
ans =

     1

';
assert_isequal(R, REF);
assert_checkerror('k = p{:}', _('Empty expression!'));
%=============================================================================
