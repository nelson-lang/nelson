%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Products = ["Tomato", "Carrot", "Mango", "Mushroom"];
Prices = [1 .5 2.50 1.99];
d = dictionary(Products, Prices)
d("Carrot")
d("Potato") = 0.75
d("Tomato") = 1.25
d("Mango") = []
d(["Celery" "Grapes"]) = [0.50 1.95]
%=============================================================================
d = dictionary("hello", "world")
d("newKey") = 1
isstring(d("newKey"))
%=============================================================================
Values = {{'a','b'},["ff", "cc"],struct,[1 2 3 4]};
Keys = ["letters" "words" "a structure" "numeric array"];
d = dictionary(Keys, Values);
assert_isequal(d{"numeric array"}, [1 2 3 4]);
d{"a new entry"} = 'table';
assert_isequal(d{"a new entry"}, 'table');
%=============================================================================