%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('det'), 1);
assert_isequal(nargout('det'), 1);
%=============================================================================
R = det([]);
REF = double(1);
assert_isequal(R, REF);
%=============================================================================
R = det(single([]));
REF = single(1);
assert_isequal(R, REF);
%=============================================================================
R = det(NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = det([1, 1; 1,NaN]);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = det([1 1; 1 NaN] + i);
REF = complex(NaN, NaN);
assert_isequal(R, REF);
%=============================================================================
R = det(Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
R = det(Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
R = det(1);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = det(1 + i);
REF = 1 + i;
assert_isequal(R, REF);
%=============================================================================
R = det([1 1; 1 2]);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = det([1 1; 1 2] + i);
REF = 1 + i;
assert_isequal(R, REF);
%=============================================================================
R = det(sparse([1 1; 1 2]));
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = det(sparse([1 1; 1 2] + i));
REF = 1 + i;
assert_isequal(R, REF);
%=============================================================================
R = det(0);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
R = det([1 2 ; 1 2]);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = det([1 2])', _('Square matrix expected.'));
%=============================================================================
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'det');
assert_checkerror('R = det(''hello'')', msg);
%=============================================================================
