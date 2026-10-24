%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
txt = evalc('RandStream.list');
asserts.istrue(~isempty(strfind(txt, 'mt19937ar')));
asserts.istrue(~isempty(strfind(txt, 'mlfg6331_64')));
asserts.istrue(~isempty(strfind(txt, 'twister64')));
%=============================================================================
% NormalTransform 'Inversion' applies the inverse normal CDF to uniform draws.
s = RandStream('mt19937ar', 'Seed', 42, 'NormalTransform', 'Inversion');
asserts.isequal(s.NormalTransform, 'Inversion');
rInv = randn(s, 1, 4);
sU = RandStream('mt19937ar', 'Seed', 42);
REF = norminv(rand(sU, 1, 4));
asserts.isapprox(rInv, REF, 1e-12);
%=============================================================================
