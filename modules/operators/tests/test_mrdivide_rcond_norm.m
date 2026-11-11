%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% b / A estimates RCOND in the infinity norm of A, A \ b in its 1-norm.
A = single([3+4i 1-2i 2; 1+1i 2i 3-1i; 4+5i 1 5-1i+2e-15]);
lastwarn('', '');
x = single([1 2 3]) / A;
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.istrue(contains(msg, '3.178369e-09'));
asserts.isequal(class(x), 'single');
%=============================================================================
lastwarn('', '');
x = single([1 2 3; 4 5 6]) / A;
[msg, id] = lastwarn();
asserts.istrue(contains(msg, '3.178369e-09'));
asserts.isequal(size(x), [2 3]);
%=============================================================================
% mixed classes take the transposed route and give the same estimate
lastwarn('', '');
x = [1 2 3] / A;
msg = lastwarn();
asserts.istrue(contains(msg, '3.178369e-09'));
lastwarn('', '');
x = single([1 2 3]) / double(A);
msg = lastwarn();
asserts.istrue(contains(msg, '3.178369e-09'));
%=============================================================================
lastwarn('', '');
x = A \ single([1; 2; 3]);
msg = lastwarn();
asserts.istrue(contains(msg, '3.266656e-09'));
%=============================================================================
lastwarn('', '');
x = [1 2 3] / double(A);
msg = lastwarn();
asserts.istrue(contains(msg, '2.368070e-17'));
%=============================================================================
