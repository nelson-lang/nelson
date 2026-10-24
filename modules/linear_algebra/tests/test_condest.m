%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
asserts.isequal(nargin('condest'), -1);
asserts.isequal(nargout('condest'), -1);
%=============================================================================
A = sparse([4 1 0; 2 3 1; 0 1 2]);
[c, v] = condest(A);
asserts.isapprox(c, 5.625, 1e-12);
asserts.isequal(size(v), [3 1]);
asserts.isapprox(norm(v, 1), 1, 1e-12);
asserts.isapprox(norm(A * v, 1), norm(A, 1) / c, 1e-12);
%=============================================================================
asserts.isapprox(condest(A, 3), 5.625, 1e-12);
asserts.isapprox(condest(A, []), 5.625, 1e-12);
asserts.isapprox(condest(A, 2i), 5.625, 1e-12);
asserts.isapprox(condest(A, -2), 5.625, 1e-12);
%=============================================================================
B = sparse([1 2; 3 4]);
[cb, vb] = condest(B);
asserts.isapprox(cb, 21, 1e-12);
asserts.isapprox(vb, [-0.5714285714285714; 0.42857142857142855], 1e-12);
asserts.isapprox(norm(vb, 1), 1, 1e-12);
asserts.isapprox(norm(B * vb, 1), norm(B, 1) / cb, 1e-12);
%=============================================================================
BN = sparse([10 -2 0 1; 3 9 4 0; 0 1 7 -3; 2 0 5 8]);
[cbn, vbn] = condest(BN);
asserts.isapprox(cbn, 3.95249406175772, 1e-12);
asserts.isequal(size(vbn), [4 1]);
asserts.isapprox(norm(vbn, 1), 1, 1e-12);
asserts.isapprox(norm(BN * vbn, 1), norm(BN, 1) / cbn, 1e-12);
[cbn3, vbn3] = condest(BN, 3);
asserts.isapprox(cbn3, 3.95249406175772, 1e-12);
asserts.isequal(size(vbn3), [4 1]);
asserts.isapprox(norm(vbn3, 1), 1, 1e-12);
asserts.isapprox(norm(BN * vbn3, 1), norm(BN, 1) / cbn3, 1e-12);
%=============================================================================
C = sparse([1 1; 1 1]);
[cc, vc] = condest(C);
asserts.isequal(cc, Inf);
asserts.isapprox(vc, [0.5; 0.5], 1e-12);
%=============================================================================
asserts.isequal(condest(sparse(0)), Inf);
[ce, ve] = condest(sparse([]));
asserts.isequal(ce, 0);
asserts.isequal(size(ve), [0 1]);
asserts.isequal(condest(sparse([]), 0), 0);
asserts.isequal(condest(sparse([]), 2), 0);
asserts.isequal(condest(speye(3)), 1);
%=============================================================================
D = single([1 0; 0 2]);
asserts.isapprox(condest(D), 2, 1e-6);
%=============================================================================
AS = sparse(single([4 1 0; 2 3 1; 0 1 2]));
[cs, vs] = condest(AS);
asserts.isequal(class(cs), 'single');
asserts.isapprox(cs, single(5.625), 1e-5);
asserts.isequal(class(vs), 'single');
asserts.isequal(size(vs), [3 1]);
asserts.isapprox(norm(vs, 1), single(1), 1e-5);
asserts.isapprox(norm(AS * vs, 1), norm(AS, 1) / cs, 1e-5);
%=============================================================================
E = sparse([1 + 1i 0; 0 2]);
ce = condest(E);
asserts.isapprox(ce, sqrt(2), 1e-12);
%=============================================================================
ES = sparse(single([1 + 1i 0; 0 2]));
[ces, ves] = condest(ES);
asserts.isequal(class(ces), 'single');
asserts.isapprox(ces, single(sqrt(2)), 1e-5);
asserts.isequal(class(ves), 'single');
asserts.isequal(size(ves), [2 1]);
asserts.isapprox(norm(ves, 1), single(1), 1e-5);
%=============================================================================
EZ = sparse([1 2 1 2], [1 1 2 2], single([0 -0 complex(0, 0) complex(0, 2)]), 2, 2, 4);
[cez, vez] = condest(EZ);
asserts.isequal(class(cez), 'single');
asserts.isequal(cez, single(Inf));
asserts.isequal(class(vez), 'single');
asserts.isapprox(vez, single([0.5; 0.5]), 1e-6);
%=============================================================================
F = sparse([2 + 1i 1; 1 - 2i 3]);
[cf, vf] = condest(F);
asserts.isapprox(cf, 3.311580158474123, 1e-12);
REF_VF = [0.40513613510659 - 0.40513613510659i; 0.135045378368863 + 0.40513613510659i];
asserts.isapprox(vf, REF_VF, 1e-12);
asserts.isapprox(norm(vf, 1), 1, 1e-12);
asserts.isapprox(norm(F * vf, 1), norm(F, 1) / cf, 1e-12);
%=============================================================================
G = sparse([1 + 1i 2; 2 4 - 1i]);
[cg, vg] = condest(G);
asserts.isapprox(cg, 11.856145010515716, 1e-12);
REF_VG = [0.0516449960774503 - 0.671384949006854i; -0.103289992154901 + 0.309869976464702i];
asserts.isapprox(vg, REF_VG, 1e-12);
asserts.isapprox(norm(vg, 1), 1, 1e-12);
%=============================================================================
H = sparse([6 0 1 0;
            2 5 0 1;
            0 1 4 2;
            1 0 0 3]);
asserts.isapprox(condest(H, 2), 5.112, 1e-12);
%=============================================================================
asserts.checkerror('condest(ones(2, 3))', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('condest(ones(2, 2, 2))', sprintf(_('%s expected.'), 'An numeric 2-D matrix'));
asserts.checkerror('condest(logical(eye(2)))', sprintf(_('%s expected.'), 'An numeric 2-D matrix'));
asserts.checkerror('condest(sparse(logical(eye(2))))', sprintf(_('%s expected.'), 'An numeric 2-D matrix'));
asserts.checkerror('condest(eye(2), 0)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(2), 1.5)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(2), 3)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(3), Inf)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(3), NaN)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(3), 1 + 2i)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(3), 2.5i)', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
asserts.checkerror('condest(eye(3), {1})', _('T must be an integer between 1 and N = MAX(SIZE(A)).'));
%=============================================================================
