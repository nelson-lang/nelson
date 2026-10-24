%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(mean(sparse([1 2 3])), sparse(2));
asserts.isequal(mean([1 NaN 3],'omitmissing'), 2);
asserts.isequal(mean([1 NaN 3],'includemissing'), NaN);
asserts.isequal(mean(sparse([1 2;3 4]),2), sparse(reshape([1.5 3.5], [2 1])));
asserts.isequal(mean(ones(2,3),'all','omitmissing'), 1);
asserts.isequal(mean(single([1 2 3]),'omitmissing'), single(2));
asserts.isequal(mean(int8([1 2 3]),'omitmissing'), 2);
asserts.isequal(mean([1 NaN 3],2,'omitmissing'), 2);
asserts.isequal(mean(sparse([1 NaN 3]),'omitnan'), sparse(2));
asserts.isequal(mean(sparse([1 NaN 3])), sparse(NaN));
asserts.isequal(mean(sparse(eye(3))), sparse([0.33333333333333331 0.33333333333333331 0.33333333333333331]));
asserts.isequal(mean(sparse(eye(3)),2), sparse(reshape([0.33333333333333331 0.33333333333333331 0.33333333333333331], [3 1])));
asserts.isequal(mean(sparse(eye(3)),'all'), sparse(0.33333333333333331));
asserts.isequal(mean(sparse(eye(3)),3), sparse(reshape([1 0 0 0 1 0 0 0 1], [3 3])));
asserts.isequal(mean(sparse(true(2))), sparse([1 1]));
asserts.isequal(mean(sparse(1+2i)), sparse(complex(1, 2)));
asserts.isequal(mean([1 NaN;3 4],'all','omitmissing'), 2.6666666666666665);
asserts.isequal(mean([1 NaN;3 4],1,'omitmissing'), [2 4]);
asserts.isequal(mean([1 NaN;3 4],2,'omitmissing'), reshape([1 3.5], [2 1]));
%=============================================================================
