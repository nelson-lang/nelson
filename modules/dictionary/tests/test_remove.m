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
R = remove(d, 2);
REF = dictionary([1 3], ["Unicycle" "Tricycle"]);
assert_isequal(R, REF);
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
R = remove(d, 4);
assert_isequal(R, d);
%=============================================================================
