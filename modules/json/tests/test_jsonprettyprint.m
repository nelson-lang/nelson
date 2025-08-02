%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
field1 = 'f1';  value1 = zeros(1,10);
field2 = 'f2';  value2 = {'a', 'b'};
field3 = 'f3';  value3 = {pi, pi*pi};
field4 = 'f4';  value4 = {'fourth'};
s = struct(field1,value1,field2,value2,field3,value3,field4,value4);
r = jsonprettyprint(jsonencode(s));
ref = '[
    {
        "f1":[
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0
        ],
        "f2":"a",
        "f3":3.141592653589793,
        "f4":"fourth"
    },
    {
        "f1":[
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0
        ],
        "f2":"b",
        "f3":9.869604401089358,
        "f4":"fourth"
    }
]';
assert_isequal(r, ref);
%=============================================================================
r = jsonencode([1, NaN, Inf, -Inf],'ConvertInfAndNaN', true);
s = jsonprettyprint(r);
ref = '[
    1,
    NaN,
    Inf,
    -Inf
]';
assert_isequal(s, ref);
%=============================================================================
s = jsonprettyprint('[ 1 ,2 , "nel son", " "]');
ref = '[
    1,
    2,
    "nel son",
    " "
]';
assert_isequal(s, ref);
%=============================================================================
