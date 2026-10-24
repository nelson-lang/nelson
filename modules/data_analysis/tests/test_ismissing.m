%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ismissing'), 1);
asserts.isequal(nargout('ismissing'), 1);
%=============================================================================
A = ["Nel", NaN, "son"];
R = ismissing(A);
REF = [false, true, false];
asserts.isequal(R, REF);
%=============================================================================
B = [1 2 NaN Inf];
R = ismissing(B);
REF = [false false true false];
asserts.isequal(R, REF);
%=============================================================================
C = 'Nel son';
R = ismissing(C);
REF = [false false false true false false false];
asserts.isequal(R, REF);
%=============================================================================
D = {'Nel' '' 'son'};
R = ismissing(D);
REF = [false true false];
asserts.isequal(R, REF);
%=============================================================================
E = missing;
R = ismissing(E);
REF = true;
asserts.isequal(R, REF);
%=============================================================================
F = repmat(missing, 2, 3);
R = ismissing(F);
REF = repmat(true, 2, 3);
asserts.isequal(R, REF);
%=============================================================================
D = [1 NaN; 2 3; NaN 4];
asserts.isequal(ismissing(D), logical([0 1; 0 0; 1 0]));
asserts.isequal(ismissing(["Nel", NaN, "son"]), [false true false]);
asserts.isequal(ismissing({'Nel' '' 'son'}), [false true false]);
%=============================================================================
