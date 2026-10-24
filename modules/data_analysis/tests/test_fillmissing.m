%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1; NaN; 3];
R = fillmissing(A, 'constant', 0);
asserts.isequal(R, [1; 0; 3]);
%=============================================================================
A = [1; NaN; NaN; 4];
R = fillmissing(A, 'previous');
asserts.isequal(R, [1; 1; 1; 4]);
%=============================================================================
[R, TF] = fillmissing([1; NaN; 3], 'linear');
asserts.isequal(R, [1; 2; 3]);
asserts.isequal(TF, [false; true; false]);
%=============================================================================
[F, TF] = fillmissing([1; NaN; 3], 'linear');
asserts.isequal(F, [1; 2; 3]);
asserts.isequal(TF, [false; true; false]);
asserts.isequal(fillmissing([1; NaN; NaN; 4], 'previous'), [1; 1; 1; 4]);
asserts.isequal(fillmissing([1; NaN; 3], 'constant', 0), [1; 0; 3]);
%=============================================================================
r = fillmissing(["a", missing, "c"], 'constant', "Z");
asserts.isequal(r, ["a", "Z", "c"]);
asserts.isequal(ismissing(r), [false, false, false]);
%=============================================================================
r = fillmissing(["a", missing, "c"], 'constant', 'Q');
asserts.isequal(r, ["a", "Q", "c"]);
%=============================================================================
r = fillmissing(["a", missing, "c"], 'constant', "Z", 'MissingLocations', [false, true, false]);
asserts.isequal(r, ["a", "Z", "c"]);
%=============================================================================
