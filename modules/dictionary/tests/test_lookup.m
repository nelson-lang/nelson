%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels,names);
R = lookup(d, 1);
assert_isequal(R, "Unicycle");
%=============================================================================
R = lookup(d, [2 3]);
assert_isequal(R, ["Bicycle"    "Tricycle"]);
%=============================================================================
R = lookup(d, [3,5], 'FallbackValue', "Wheeled Vehicle");
REF =  [ "Tricycle"    "Wheeled Vehicle"];
assert_isequal(R, REF);
%=============================================================================
names = {"Unicycle" "Bicycle" "Tricycle"};
wheels = [1 2 3];
d = dictionary(wheels,names);
R = lookup(d,[3, 5, 1], 'FallbackValue', {3});
REF =   {"Tricycle", 3 , "Unicycle"};
assert_isequal(R, REF);
%=============================================================================