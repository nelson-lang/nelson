%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
wheels = [1 2 3];
names = ["Monocycle" "Bicycle" "Tricycle"];
d = dictionary(wheels, names)
dict = pyrun("A = d", "A", 'd', d);
R = dictionary(dict);
assert_isequal(d, R);
%=============================================================================
