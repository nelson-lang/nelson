%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(median(sparse([1 2 3 4])), sparse(2.5));
asserts.isequal(median([true false true]), true);
asserts.isequal(median([1 NaN 3],'omitmissing'), 2);
asserts.isequal(median([1 NaN 3],'includemissing'), NaN);
asserts.isequal(median([realmax realmax]), 1.7976931348623157e+308);
asserts.isequal(median([-realmax -realmax]), -1.7976931348623157e+308);
asserts.isequal(median(sparse(ones(2,3)),2), sparse(reshape([1 1], [2 1])));
asserts.isequal(median(true), true);
asserts.isequal(median('a'), char([97]));
asserts.isequal(median([true true false false]), true);
asserts.isequal(median(sparse([])), sparse(NaN));
asserts.isequal(median(sparse(0,3)), sparse([NaN NaN NaN]));
asserts.isequal(median([true false;true true]), logical([1 1]));
asserts.isequal(median('abc'), char([98]));
asserts.isequal(median(logical([1 0 0 1])), true);
asserts.isequal(median(logical([1 1 0 0])), true);
asserts.isequal(median(sparse([1 2;3 4])), sparse([2 3]));
asserts.isequal(median(sparse([1 2;3 4]),2), sparse(reshape([1.5 3.5], [2 1])));
asserts.isequal(median(sparse([1 NaN 3]),'omitnan'), sparse(2));
asserts.isequal(median(sparse([1 NaN 3])), sparse(NaN));
asserts.isequal(median(sparse(1:4),'all'), sparse(2.5));
asserts.isequal(median([NaN NaN],'omitmissing'), NaN);
asserts.isequal(median([realmax realmax realmax realmax]), 1.7976931348623157e+308);
asserts.isequal(median([1e308 1.5e308]), 1.25e+308);
asserts.isequal(median([-1e308 -1.5e308]), -1.25e+308);
asserts.isequal(median([1 NaN;3 4],'omitmissing'), [2 4]);
asserts.isequal(median([1 NaN;3 4],2,'omitmissing'), reshape([1 3.5], [2 1]));
asserts.isequal(median([1 NaN 3 4],'all','omitmissing'), 3);
asserts.isequal(median([true false false]), false);
asserts.isequal(median([true true false]), true);
asserts.isequal(median([true false]), true);
asserts.isequal(median(logical([0 0 1 1])), true);
asserts.isequal(median(true(2,2)), logical([1 1]));
%=============================================================================
