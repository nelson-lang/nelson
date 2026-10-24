%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
R = lookup(d, 1);
asserts.isequal(R, "Unicycle");
%=============================================================================
R = lookup(d, [2 3]);
asserts.isequal(R, ["Bicycle" "Tricycle"]);
%=============================================================================
R = lookup(d, [3, 5], 'FallbackValue', "Wheeled Vehicle");
REF = ["Tricycle" "Wheeled Vehicle"];
asserts.isequal(R, REF);
%=============================================================================
names = {"Unicycle" "Bicycle" "Tricycle"};
wheels = [1 2 3];
d = dictionary(wheels, names);
R = lookup(d, [3, 5, 1], 'FallbackValue', {3});
REF = {"Tricycle", 3, "Unicycle"};
asserts.isequal(R, REF);
%=============================================================================
d = dictionary(string({'abc'}), 42);
asserts.istrue(isKey(d, 'abc'));
R = lookup(d, 'abc');
asserts.isequal(R, 42);
%=============================================================================
d = dictionary(string({'a', 'b'}), {1, [2 3]});
R = lookup(d, string({'b', 'c'}), 'FallbackValue', {9});
asserts.isequal(class(R), 'cell');
asserts.isequal(R{1}, [2 3]);
asserts.isequal(R{2}, 9);
%=============================================================================
d = dictionary({1, 2, 3}, {'one', 'two', 'three'});
R = lookup(d, {2, 4}, 'FallbackValue', {'missing'});
asserts.isequal(class(R), 'cell');
asserts.isequal(R{1}, 'two');
asserts.isequal(R{2}, 'missing');
%=============================================================================
