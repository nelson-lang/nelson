%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'long', '', 'x'; 'x', 'long', ''});
asserts.isequal(cellstr(A), {'long', '<undefined>', 'x'; 'x', 'long', '<undefined>'});
S = string(A);
asserts.isequal(size(S), size(A));
asserts.isequal(S(~ismissing(S)), ["long"; "x"; "long"; "x"]);
asserts.isequal(ismissing(S), isundefined(A));
C = char(A(:));
asserts.isequal(size(C, 1), numel(A));
asserts.istrue(size(C, 2) >= length('<undefined>'));
%=============================================================================
codes = double(A);
asserts.isequal(single(A), single(codes));
asserts.isequal(int8(A), int8(codes));
asserts.isequal(int16(A), int16(codes));
asserts.isequal(int32(A), int32(codes));
asserts.isequal(int64(A), int64(codes));
asserts.isequal(uint8(A), uint8(codes));
asserts.isequal(uint16(A), uint16(codes));
asserts.isequal(uint32(A), uint32(codes));
asserts.isequal(uint64(A), uint64(codes));
%=============================================================================
N = categorical([1 NaN Inf -Inf 1]);
asserts.isequal(categories(N), {'-Inf'; '1'; 'Inf'});
asserts.isequal(isundefined(N), [false true false false false]);
asserts.isequal(cellstr(N), {'1', '<undefined>', 'Inf', '-Inf', '1'});
%=============================================================================
L = categorical([true false true]);
asserts.isequal(categories(L), {'false'; 'true'});
asserts.isequal(cellstr(L), {'true', 'false', 'true'});
%=============================================================================
out = evalc('disp(A);');
asserts.istrue(contains(out, 'long'));
asserts.isfalse(contains(out, '"'));
asserts.istrue(contains(out, '<undefined>'));
out = evalc('display(A, ''A'');');
asserts.istrue(contains(out, 'A ='));
asserts.isfalse(contains(out, '"'));
Q = categorical({['can', char(39), 't'], 'x'});
out = evalc('disp(Q);');
asserts.istrue(contains(out, ['can', char(39), 't']));
ND = reshape(categorical({'a', 'b', 'c', ''}), [1 2 2]);
out = evalc('display(ND, ''ND'');');
asserts.istrue(contains(out, 'ND(:,:,1) ='));
asserts.istrue(contains(out, 'ND(:,:,2) ='));
%=============================================================================
