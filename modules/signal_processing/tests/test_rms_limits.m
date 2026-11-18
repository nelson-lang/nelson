%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a 0-by-0 empty reduces to a scalar NaN
R = rms([]);
asserts.isequal(class(R), 'double');
asserts.isequal(R, NaN);
%=============================================================================
% a single 0-by-0 empty keeps its class and reduces to a scalar NaN
R = rms(single([]));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single(NaN));
%=============================================================================
% reducing along a zero-length dimension yields NaN, otherwise an empty row
asserts.isequal(rms(zeros(2, 0)), zeros(1, 0));
asserts.isequal(rms(zeros(0, 3)), [NaN NaN NaN]);
%=============================================================================
% an explicit dimension follows the usual reduction rules
asserts.isequal(rms([], 1), zeros(1, 0));
asserts.isequal(rms([], 2), zeros(0, 1));
asserts.isequal(rms(zeros(0, 3), 1), [NaN NaN NaN]);
asserts.isequal(rms(zeros(0, 3), 2), zeros(0, 1));
asserts.isequal(rms(zeros(2, 0), 1), zeros(1, 0));
asserts.isequal(rms(zeros(2, 0), 2), [NaN; NaN]);
%=============================================================================
% 'all' over an empty input is NaN, over a full input is finite
asserts.isequal(rms(zeros(2, 0), 'all'), NaN);
asserts.isapprox(rms(ones(2, 3), 'all'), 1, 1e-12);
%=============================================================================
% omitting missing values keeps the same empty semantics
asserts.isequal(rms([], 'omitnan'), NaN);
asserts.isequal(rms(zeros(0, 3), 'omitnan'), [NaN NaN NaN]);
asserts.isequal(rms(zeros(2, 0), 'omitnan'), zeros(1, 0));
%=============================================================================
