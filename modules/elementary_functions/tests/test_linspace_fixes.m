%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% huge endpoints do not overflow
asserts.isequal(linspace(1e308, -1e308, 3), [1e308 0 -1e308]);
asserts.isequal(linspace(-1e308, 1e308, 5), [-1e308 -5e307 0 5e307 1e308]);
asserts.isequal(linspace(1e308, -1e308, 1), -1e308);
asserts.isequal(linspace(1e308, -1e308, 2), [1e308 -1e308]);
R = linspace(1e308, -1e308, 4);
asserts.istrue(all(isfinite(R)));
asserts.isequal(R([1 end]), [1e308 -1e308]);
R = linspace(realmax, -realmax, 7);
asserts.istrue(all(isfinite(R)));
asserts.isequal(R(4), 0);
%=============================================================================
% infinite endpoints
asserts.isequal(linspace(-Inf, Inf, 3), [-Inf 0 Inf]);
asserts.isequal(linspace(-Inf, Inf, 5), [-Inf -Inf 0 Inf Inf]);
asserts.isequal(linspace(Inf, -Inf, 3), [Inf 0 -Inf]);
asserts.isequal(linspace(-Inf, -Inf, 3), [-Inf -Inf -Inf]);
asserts.isequal(linspace(Inf, Inf, 3), [Inf Inf Inf]);
asserts.isequal(linspace(-Inf, 2, 3), [-Inf NaN 2]);
asserts.isequal(linspace(1, Inf, 3), [1 Inf Inf]);
asserts.isequal(linspace(Inf, 1, 3), [Inf NaN 1]);
asserts.isequal(linspace(2, -Inf, 3), [2 -Inf -Inf]);
%=============================================================================
% number of points
asserts.isequal(linspace(1, 10, NaN), NaN);
asserts.isequal(class(linspace(1, 10, NaN)), 'double');
asserts.isequal(linspace(single(1), 10, NaN), single(NaN));
asserts.isequal(class(linspace(single(1), 10, NaN)), 'single');
asserts.isequal(linspace(1, 10, -Inf), zeros(1, 0));
asserts.isequal(linspace(1, 10, -1), zeros(1, 0));
asserts.isequal(linspace(1, 10, 2.7), [1 10]);
assert_checkerror('linspace(1, 10, Inf)', _('Requested array exceeds the maximum possible variable size.'));
R = linspace(1, 10, 'a');
asserts.isequal(size(R), [1 97]);
asserts.isequal(R([1 end]), [1 10]);
asserts.isequal(linspace(1, 10, true), 10);
asserts.isequal(linspace(1, 10, int8(5)), [1 3.25 5.5 7.75 10]);
%=============================================================================
asserts.isequal(linspace(1, 10, 5), [1 3.25 5.5 7.75 10]);
asserts.isequal(linspace(0, 1e308, 3), [0 5e307 1e308]);
asserts.isequal(linspace(-1e308, 0, 3), [-1e308 -5e307 0]);
%=============================================================================
