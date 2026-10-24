%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Shapes produced by the array reductions of jsondecode.
%=============================================================================
% matrices of objects are row-wise, N-D arrays of objects element-wise
r = jsondecode('[[{"a":1},{"a":2}],[{"a":3},{"a":4}]]');
asserts.isequal(size(r), [2 2]);
asserts.isequal(reshape([r.a], 2, 2), [1 2; 3 4]);
r = jsondecode('[[[{"a":1},{"a":2}],[{"a":3},{"a":4}]],[[{"a":5},{"a":6}],[{"a":7},{"a":8}]]]');
asserts.isequal(size(r), [2 2 2]);
asserts.isequal([r.a], [1 3 2 4 5 7 6 8]);
r = jsondecode('[[{"a":1}],[{"a":2}]]');
asserts.isequal(size(r), [2 1]);
%=============================================================================
% same keys in another order: struct array in the order of the first object
r = jsondecode('[{"b":1,"a":2},{"a":3,"b":4}]');
asserts.isequal(fieldnames(r), {'b'; 'a'});
asserts.isequal([r.b], [1 4]);
asserts.isequal([r.a], [2 3]);
r = jsondecode('[{"a":1},{"b":2}]');
asserts.isequal(class(r), 'cell');
%=============================================================================
% null: NaN in a double array, [] elsewhere
asserts.isequal(jsondecode('[[1,2],[null,3]]'), [1 2; NaN 3]);
asserts.isequal(jsondecode('[null, null]'), {[]; []});
asserts.isequal(jsondecode('[[null,null],[1,2]]'), {{[]; []}; [1; 2]});
asserts.isequal(jsondecode('[[1,2],[3]]'), {[1; 2]; 3});
%=============================================================================
asserts.isequal(jsondecode('[[[1],[2]],[[3],[4]]]'), reshape([1 3 2 4], 2, 1, 2));
asserts.isequal(jsondecode('[{}, {}]'), repmat(struct(), 2, 1));
asserts.isequal(jsondecode('[[], []]'), {cell(0, 1); cell(0, 1)});
%=============================================================================
% -0 is kept
r = jsondecode('[-0, 0]');
asserts.isequal(1 ./ r, [-Inf; Inf]);
%=============================================================================
% duplicate keys in a large object
keys = arrayfun(@(k) sprintf('"k%d":%d', k, k), 0:39, 'UniformOutput', false);
r = jsondecode(['{', strjoin(keys, ','), ',"k5":100,"k5":101,"k5_1":102}']);
names = fieldnames(r);
asserts.isequal(names(end - 2:end), {'k5_1'; 'k5_2'; 'k5_1_1'});
asserts.isequal([r.k5_1, r.k5_2, r.k5_1_1], [100 101 102]);
r = jsondecode(['{"a":1,"a_1":2,', strjoin(keys, ','), ',"a":3,"a":4}']);
asserts.isequal([r.a, r.a_1, r.a_2, r.a_3], [1 2 3 4]);
r = jsondecode(['[', repmat(['{', strjoin(keys, ','), ',"k7":7},'], 1, 2), '{', strjoin(keys, ','), ',"k7":8}]']);
asserts.isequal(size(r), [3 1]);
asserts.isequal([r.k7_1], [7 7 8]);
%=============================================================================
