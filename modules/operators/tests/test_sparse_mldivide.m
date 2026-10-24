%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([4 -1 0 0; -1 4 -1 0; 0 -1 4 -1; 0 0 -1 3]);
b = [15; 10; 10; 10];
x = A \ b;
asserts.isapprox(x, [5; 5; 5; 5], 1e-12);
asserts.isapprox(A * x, b, 1e-12);
%=============================================================================
As = sparse(single([4 -1 0 0; -1 4 -1 0; 0 -1 4 -1; 0 0 -1 3]));
bs = single([15; 10; 10; 10]);
xs = As \ bs;
asserts.isequal(class(xs), 'single');
asserts.isapprox(xs, single([5; 5; 5; 5]), 1e-5);
asserts.isapprox(As * xs, bs, 1e-4);
Mref = sparse(single([4 1 0; 2 3 1; 0 1 2]));
xMixed = Mref \ ones(3, 1);
asserts.isequal(class(xMixed), 'single');
asserts.isapprox(xMixed, single([0.25; 0; 0.5]), 1e-6);
%=============================================================================
Bsparse = sparse([15 1; 10 0; 10 2; 10 0]);
Xsparse = A \ Bsparse;
REF_SPD_SPARSE_RHS = [5 0.30718954248366; 5 0.22875816993464; 5 0.607843137254902; 5 0.202614379084967];
asserts.istrue(issparse(Xsparse));
asserts.isapprox(full(Xsparse), REF_SPD_SPARSE_RHS, 1e-12);
asserts.isapprox(full(A * Xsparse), full(Bsparse), 1e-12);
BsparseSingle = sparse(single([15 1; 10 0; 10 2; 10 0]));
XsparseSingle = As \ BsparseSingle;
asserts.istrue(issparse(XsparseSingle));
asserts.isequal(class(XsparseSingle), 'single');
asserts.isapprox(full(XsparseSingle), single(REF_SPD_SPARSE_RHS), 1e-5);
asserts.isapprox(full(As * XsparseSingle), full(BsparseSingle), 1e-4);
%=============================================================================
AcomplexSpd = sparse([4 1 + 1i 0; 1 - 1i 5 2; 0 2 6]);
bcomplexSpd = [1 + 2i; 3 - 1i; 4];
xcomplexSpd = AcomplexSpd \ bcomplexSpd;
REF_COMPLEX_SPD = [0.108695652173913 + 0.521739130434783i;
    0.239130434782609 - 0.326086956521739i;
    0.58695652173913 + 0.108695652173913i];
asserts.isapprox(xcomplexSpd, REF_COMPLEX_SPD, 1e-12);
asserts.isapprox(AcomplexSpd * xcomplexSpd, bcomplexSpd, 1e-12);

BcomplexSpd = sparse([1 + 2i 0; 3 - 1i 1; 4 2i]);
XcomplexSpd = AcomplexSpd \ BcomplexSpd;
REF_COMPLEX_SPD_SPARSE_RHS = [0.108695652173913 + 0.521739130434783i -0.108695652173913 - 0.0217391304347826i;
    0.239130434782609 - 0.326086956521739i 0.260869565217391 - 0.173913043478261i;
    0.58695652173913 + 0.108695652173913i -0.0869565217391304 + 0.391304347826087i];
asserts.istrue(issparse(XcomplexSpd));
asserts.isapprox(full(XcomplexSpd), REF_COMPLEX_SPD_SPARSE_RHS, 1e-12);
asserts.isapprox(full(AcomplexSpd * XcomplexSpd), full(BcomplexSpd), 1e-12);
AcomplexSpds = sparse(single([4 1 + 1i 0; 1 - 1i 5 2; 0 2 6]));
BcomplexSpds = sparse(single([1 + 2i 0; 3 - 1i 1; 4 2i]));
XcomplexSpds = AcomplexSpds \ BcomplexSpds;
asserts.istrue(issparse(XcomplexSpds));
asserts.isequal(class(XcomplexSpds), 'single');
asserts.isapprox(full(XcomplexSpds), single(REF_COMPLEX_SPD_SPARSE_RHS), 1e-5);
asserts.isapprox(full(AcomplexSpds * XcomplexSpds), full(BcomplexSpds), 1e-4);
%=============================================================================
S = sparse([0 1; 1 0]);
xs = S \ [1; 2];
asserts.isapprox(xs, [2; 1], 1e-12);
asserts.isapprox(S * xs, [1; 2], 1e-12);
%=============================================================================
Hindef = sparse(single([0 1 + 2i; 1 - 2i 3]));
Bindef = sparse(single([1 + 1i 0; 2 - 1i 1]));
Xindef = Hindef \ Bindef;
REF_HINDEF = single([0.2 0.2 + 0.4i; 0.6 - 0.2i 0]);
asserts.istrue(issparse(Xindef));
asserts.isequal(class(Xindef), 'single');
asserts.isapprox(full(Xindef), REF_HINDEF, 1e-5);
asserts.isapprox(full(Hindef * Xindef), full(Bindef), 1e-4);
%=============================================================================
R = sparse([1 0; 0 1; 1 1; 2 -1]);
c = [1; 2; 4; 1];
xr = R \ c;
asserts.isapprox(xr, [1.5294117647058827; 2.1764705882352939], 1e-12);
asserts.isapprox(norm(R * xr - c), 0.64168894791974773, 1e-12);
%=============================================================================
Areal = sparse(eye(2, 2));
brhs = [1 + 1i; 2];
xmix = Areal \ brhs;
asserts.isapprox(xmix, brhs, 1e-12);
%=============================================================================
Cmix = sparse([3 + 1i 1; 0 2 - 1i]);
rmix = [4; 3];
xcmix = Cmix \ rmix;
asserts.isapprox(xcmix, [0.78 - 0.46i; 1.2 + 0.6i], 1e-12);
asserts.istrue(norm(Cmix * xcmix - rmix) < 1e-12);
%=============================================================================
Rmix = sparse([1 0; 0 1; 1 1; 2 -1]);
ymix = [1 + 1i; 2; 4 - 2i; 1 + 3i];
xrmix = Rmix \ ymix;
REF_RMIX = [1.5294117647058827 + 0.5882352941176471i; 2.1764705882352939 - 1.4705882352941178i];
asserts.isapprox(xrmix, REF_RMIX, 1e-12);
asserts.isapprox(norm(Rmix * xrmix - ymix), 2.029198624783569, 1e-12);
%=============================================================================
C = sparse([3 + 1i 1; 0 2 - 1i]);
d = [4 + 2i; 3 - 1i];
xc = C \ d;
asserts.isapprox(xc, [0.96 + 0.28i; 1.4 + 0.2i], 1e-12);
asserts.isapprox(C * xc, d, 1e-12);
Cs = sparse(single([3 + 1i 1; 0 2 - 1i]));
ds = single([4 + 2i; 3 - 1i]);
xcs = Cs \ ds;
asserts.isequal(class(xcs), 'single');
asserts.isapprox(xcs, single([0.96 + 0.28i; 1.4 + 0.2i]), 1e-5);
asserts.isapprox(Cs * xcs, ds, 1e-4);
Ys = sparse(single([4 + 2i 0; 3 - 1i 1]));
XcsSparse = Cs \ Ys;
REF_CS_SPARSE_RHS = single([0.96 + 0.28i -0.14 - 0.02i; 1.4 + 0.2i 0.4 + 0.2i]);
asserts.istrue(issparse(XcsSparse));
asserts.isequal(class(XcsSparse), 'single');
asserts.isapprox(full(XcsSparse), REF_CS_SPARSE_RHS, 1e-5);
asserts.isapprox(full(Cs * XcsSparse), full(Ys), 1e-4);
zs = sparse(single([1 + 1i 0; 0 2]));
xzs = zs \ single([0.5; 0.5]);
asserts.isequal(class(xzs), 'single');
asserts.isapprox(xzs, single([0.25 - 0.25i; 0.25]), 1e-6);
%=============================================================================
Crect = sparse([1 + 1i 0; 0 2; 1 -1i]);
drect = [1; 2; 3 + 1i];
xcrect = Crect \ drect;
REF_CRECT = [1.214285714285714 + 0.214285714285714i; 0.642857142857143 + 0.357142857142857i];
asserts.isapprox(xcrect, REF_CRECT, 1e-12);
asserts.isapprox(norm(Crect * xcrect - drect), 2.6726124191242437, 1e-12);
%=============================================================================
BrectComplexSparse = sparse([1 0; 2 + 1i 1; 3 + 1i 2i]);
XrectComplexSparse = Crect \ BrectComplexSparse;
REF_CRECT_SPARSE_RHS = [1.07142857142857 + 0.214285714285714i 0 + 0.714285714285714i;
    0.642857142857143 + 0.785714285714286i 0.142857142857143 + 0i];
asserts.istrue(issparse(XrectComplexSparse));
asserts.isapprox(full(XrectComplexSparse), REF_CRECT_SPARSE_RHS, 1e-12);
asserts.isapprox(norm(full(Crect * XrectComplexSparse - BrectComplexSparse), 'fro'), 3.07059789431495, 1e-12);
Crects = sparse(single([1 + 1i 0; 0 2; 1 -1i]));
drects = single([1; 2; 3 + 1i]);
xcrects = Crects \ drects;
asserts.isequal(class(xcrects), 'single');
asserts.isapprox(xcrects, single(REF_CRECT), 1e-5);
BrectComplexSparseSingle = sparse(single([1 0; 2 + 1i 1; 3 + 1i 2i]));
XrectComplexSparseSingle = Crects \ BrectComplexSparseSingle;
asserts.istrue(issparse(XrectComplexSparseSingle));
asserts.isequal(class(XrectComplexSparseSingle), 'single');
asserts.isapprox(full(XrectComplexSparseSingle), single(REF_CRECT_SPARSE_RHS), 1e-5);
asserts.isapprox(norm(full(Crects * XrectComplexSparseSingle - BrectComplexSparseSingle), 'fro'), single(3.0705978870391846), 1e-5);
%=============================================================================
M = sparse([4 1 0; 2 3 1; 0 1 2]);
Y = sparse([1 0; 2 1; 3 0]);
X = M \ Y;
REF = [0.25 -0.125; 0 0.5; 1.5 -0.25];
asserts.istrue(issparse(X));
asserts.isapprox(full(X), REF, 1e-12);
asserts.isapprox(full(M * X), full(Y), 1e-12);
Ms = sparse(single([4 1 0; 2 3 1; 0 1 2]));
Ys = sparse(single([1 0; 2 1; 3 0]));
Xs = Ms \ Ys;
asserts.istrue(issparse(Xs));
asserts.isequal(class(Xs), 'single');
asserts.isapprox(full(Xs), single(REF), 1e-5);
asserts.isapprox(full(Ms * Xs), full(Ys), 1e-4);
%=============================================================================
Ycomplex = sparse([1 + 1i 0; 2 1 - 2i; 3i 0]);
Xcomplex = M \ Ycomplex;
REF_COMPLEX = [0.0625 + 0.5i -0.125 + 0.25i;
    0.75 - 1i 0.5 - 1i;
    -0.375 + 2i -0.25 + 0.5i];
asserts.istrue(issparse(Xcomplex));
asserts.isapprox(full(Xcomplex), REF_COMPLEX, 1e-12);
asserts.isapprox(full(M * Xcomplex), full(Ycomplex), 1e-12);
%=============================================================================
Yrect = sparse([1 0; 2 1; 4 0; 1 0]);
Xrect = R \ Yrect;
REF_RECT = [1.5294117647058827 0.0588235294117647; 2.1764705882352939 0.35294117647058826];
asserts.istrue(issparse(Xrect));
asserts.isapprox(full(Xrect), REF_RECT, 1e-12);
asserts.isapprox(norm(full(R * Xrect - Yrect), 'fro'), 1.0289915108550531, 1e-12);
Rs = sparse(single([1 0; 0 1; 1 1; 2 -1]));
Yrects = sparse(single([1 0; 2 1; 4 0; 1 0]));
Xrects = Rs \ Yrects;
asserts.istrue(issparse(Xrects));
asserts.isequal(class(Xrects), 'single');
asserts.isapprox(full(Xrects), single(REF_RECT), 1e-5);
asserts.isapprox(norm(full(Rs * Xrects) - full(Yrects), 'fro'), single(1.0289915108550531), 1e-5);
%=============================================================================
Aunder = sparse([1 2 0; 0 4 3]);
dunder = sparse([8; 18]);
xunder = Aunder \ dunder;
asserts.istrue(issparse(xunder));
asserts.isapprox(full(xunder), [8; 0; 6], 1e-12);
asserts.isapprox(full(Aunder * xunder), full(dunder), 1e-12);
%=============================================================================
dunderComplex = [8 + 2i; 18 - 3i];
xunderComplex = Aunder \ dunderComplex;
asserts.isapprox(xunderComplex, [8 + 2i; 0; 6 - 1i], 1e-12);
asserts.isapprox(Aunder * xunderComplex, dunderComplex, 1e-12);

AunderSingle = sparse(single([1 2 0; 0 4 3]));
dunderSingleComplex = single([8 + 2i; 18 - 3i]);
xunderSingleComplex = AunderSingle \ dunderSingleComplex;
asserts.isequal(class(xunderSingleComplex), 'single');
asserts.isapprox(xunderSingleComplex, single([8 + 2i; 0; 6 - 1i]), 1e-5);
asserts.isapprox(AunderSingle * xunderSingleComplex, dunderSingleComplex, 1e-4);
%=============================================================================
m = 1300;
n = 450;
d = ones(n, 1);
AoverLarge = sparse(1:n, 1:n, 4 * d, m, n) ...
  + sparse(2:n, 1:n - 1, d(2:n), m, n) ...
  + sparse(1:n - 1, 2:n, 0.5 * d(1:n - 1), m, n) ...
  + sparse(n + 1:2 * n, 1:n, 0.25 * d, m, n);
xrefOverLarge = ones(n, 1);
boverLarge = AoverLarge * xrefOverLarge;
xoverLarge = AoverLarge \ boverLarge;
asserts.istrue(isfinite(norm(xoverLarge)));
asserts.istrue(norm(AoverLarge * xoverLarge - boverLarge) / norm(boverLarge) < 1e-7);

m = 450;
n = 1300;
d = ones(m, 1);
AunderLarge = sparse(1:m, 1:m, 4 * d, m, n) ...
  + sparse(2:m, 1:m - 1, d(2:m), m, n) ...
  + sparse(1:m - 1, 2:m, 0.5 * d(1:m - 1), m, n) ...
  + sparse(1:m, m + 1:2 * m, 0.25 * d, m, n);
xrefUnderLarge = zeros(n, 1);
xrefUnderLarge(1:m) = 1;
dunderLarge = AunderLarge * xrefUnderLarge;
xunderLarge = AunderLarge \ dunderLarge;
asserts.istrue(isfinite(norm(xunderLarge)));
asserts.istrue(norm(AunderLarge * xunderLarge - dunderLarge) / norm(dunderLarge) < 1e-7);
%=============================================================================
G = sparse([1 2; 2 4; 3 6]);
g = [1; 2; 3];
lastwarn('');
xg = G \ g;
asserts.isapprox(xg, [1; 0], 1e-12);
asserts.istrue(norm(G * xg - g) < 1e-12);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is rank deficient to machine precision:')));
%=============================================================================
H = sparse([1 2 3; 2 4 6]);
h = [1; 2];
lastwarn('');
xh = H \ h;
asserts.isapprox(xh, [1; 0; 0], 1e-12);
asserts.istrue(norm(H * xh - h) < 1e-12);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is rank deficient to machine precision:')));
%=============================================================================
K = sparse([1 2; 2 4]);
k = [1; 2];
lastwarn('');
xk = K \ k;
asserts.isequal(isnan(xk), logical([true; true]));
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

Krhs = sparse([1 0; 2 1]);
lastwarn('');
Xk = K \ Krhs;
asserts.istrue(issparse(Xk));
FXk = full(Xk);
asserts.isequal(isnan(FXk), logical([true false; true false]));
asserts.isequal(isinf(FXk), logical([false true; false true]));
asserts.istrue(FXk(1, 2) > 0);
asserts.istrue(FXk(2, 2) < 0);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

Kc = sparse([1 1 + 1i; 2 2 + 2i]);
kc = [1 + 1i; 2 + 2i];
lastwarn('');
xkc = Kc \ kc;
asserts.isequal(isnan(real(xkc)), logical([true; true]));
asserts.isequal(isnan(imag(xkc)), logical([true; true]));
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));
%=============================================================================
Ks = sparse(single([1 2; 2 4]));
ks = single([1; 2]);
lastwarn('');
xks = Ks \ ks;
asserts.isequal(class(xks), 'single');
asserts.isequal(isnan(xks), logical([true; true]));
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));
%=============================================================================
Z = sparse([1 0; 0 0]);
lastwarn('');
xz = Z \ [1; 2];
asserts.isequal(xz, [1; Inf]);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

lastwarn('');
XzSparse = Z \ sparse([1 0; 2 0]);
asserts.istrue(issparse(XzSparse));
FXzSparse = full(XzSparse);
asserts.isequal(isinf(FXzSparse), logical([false false; true false]));
asserts.isequal(isnan(FXzSparse), logical([false false; false true]));
asserts.isequal(FXzSparse(1, 1), 1);
asserts.isequal(FXzSparse(1, 2), 0);
asserts.istrue(FXzSparse(2, 1) > 0);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));
%=============================================================================
Z0 = sparse(zeros(2, 2));
lastwarn('');
xz0 = Z0 \ [1; -2];
asserts.isequal(xz0, [Inf; -Inf]);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));
Z0s = sparse(single(zeros(2, 2)));
lastwarn('');
xz0s = Z0s \ single([1; -2]);
asserts.isequal(class(xz0s), 'single');
asserts.isequal(isinf(xz0s), logical([true; true]));
asserts.istrue(xz0s(1) > 0);
asserts.istrue(xz0s(2) < 0);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

lastwarn('');
xz0zero = Z0 \ [0; 0];
asserts.isequal(isnan(xz0zero), logical([true; true]));
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

lastwarn('');
Xz0sparse = Z0 \ sparse([1 0; -2 0]);
asserts.istrue(issparse(Xz0sparse));
FXz0sparse = full(Xz0sparse);
asserts.isequal(isinf(FXz0sparse), logical([true false; true false]));
asserts.isequal(isnan(FXz0sparse), logical([false true; false true]));
asserts.istrue(FXz0sparse(1, 1) > 0);
asserts.istrue(FXz0sparse(2, 1) < 0);
asserts.isequal(nnz(Xz0sparse), 4);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

lastwarn('');
xz0complex = Z0 \ [1 + 2i; 0 - 3i];
asserts.istrue(isinf(real(xz0complex(1))) && real(xz0complex(1)) > 0);
asserts.istrue(isinf(imag(xz0complex(1))) && imag(xz0complex(1)) > 0);
asserts.isequal(real(xz0complex(2)), 0);
asserts.istrue(isinf(imag(xz0complex(2))) && imag(xz0complex(2)) < 0);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));

lastwarn('');
Xz0complexSparse = Z0 \ sparse([1 + 2i 0; 0 - 3i 0]);
asserts.istrue(issparse(Xz0complexSparse));
FXz0complexSparse = full(Xz0complexSparse);
asserts.istrue(isinf(real(FXz0complexSparse(1, 1))) && real(FXz0complexSparse(1, 1)) > 0);
asserts.istrue(isinf(imag(FXz0complexSparse(1, 1))) && imag(FXz0complexSparse(1, 1)) > 0);
asserts.isequal(real(FXz0complexSparse(2, 1)), 0);
asserts.istrue(isinf(imag(FXz0complexSparse(2, 1))) && imag(FXz0complexSparse(2, 1)) < 0);
asserts.istrue(isnan(real(FXz0complexSparse(1, 2))));
asserts.isequal(imag(FXz0complexSparse(1, 2)), 0);
asserts.istrue(isnan(real(FXz0complexSparse(2, 2))));
asserts.isequal(imag(FXz0complexSparse(2, 2)), 0);
asserts.isequal(nnz(Xz0complexSparse), 4);
W = lastwarn();
asserts.istrue(startsWith(W, _('Matrix is singular to working precision.')));
%=============================================================================
RemptySparse = speye(3) \ sparse(zeros(3, 0));
asserts.istrue(issparse(RemptySparse));
asserts.isequal(size(RemptySparse), [3 0]);

RemptyDense = speye(3) \ zeros(3, 0);
asserts.isfalse(issparse(RemptyDense));
asserts.isequal(size(RemptyDense), [3 0]);

RemptyUnderdetermined = sparse(zeros(3, 0)) \ sparse(zeros(3, 2));
asserts.istrue(issparse(RemptyUnderdetermined));
asserts.isequal(size(RemptyUnderdetermined), [0 2]);

RemptyOverdetermined = sparse(zeros(0, 3)) \ sparse(zeros(0, 1));
asserts.istrue(issparse(RemptyOverdetermined));
asserts.isequal(size(RemptyOverdetermined), [3 1]);
asserts.isequal(nnz(RemptyOverdetermined), 0);
%=============================================================================
asserts.checkerror('sparse(ones(2, 3)) \ [1; 2; 3]', _('Requested divide operation requires arguments to have correct dimensions.'));
%=============================================================================
