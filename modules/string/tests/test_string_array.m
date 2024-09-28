%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = strings(3,3);
assert_isequal(R{1}, '');
assert_isequal(R(1), "");
assert_checkerror('R{1} = 3', _('{} assignment expects a character vector.'));
R(1) = '3';
%=============================================================================
S = ['open'; ' is '; 'not '; 'free'];
R = string(S);
REF = ["open"; " is "; "not "; "free"];
assert_isequal(R, REF);
%=============================================================================
A = 'abcdefghifklmnopqr';
B = reshape(A, 3, 2, 3);
R = string(B);
REF = ["ad",    "gf",    "mp";
"be",    "hk",    "nq";
"cf",    "il",    "or"];
assert_isequal(R, REF);
%=============================================================================
R = string({'Jan','Feb'});
R(5) = 'Jun';
REF = ["Jan", "Feb", string(NaN), string(NaN), "Jun"];
assert_isequal(R, REF);
%=============================================================================
R(5,5) = 'May';
REF = ["Jan" ,"Feb", string(NaN), string(NaN), "Jun";
string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
string(NaN), string(NaN), string(NaN), string(NaN), "May"];
assert_isequal(R, REF);
%=============================================================================
R = ["ad",    "gf",    "mp";
"be",    "hk",    "nq";
"cf",    "il",    "or"];
R(2, 2) = '';
REF = ["ad",    "gf",    "mp";
"be",    "",    "nq";
"cf",    "il",    "or"]
assert_isequal(R, REF);
%=============================================================================
R = ["ad",    "gf",    "mp";
"be",    "hk",    "nq";
"cf",    "il",    "or"];
R(2, 2) = "";
REF = ["ad",    "gf",    "mp";
"be",    "",    "nq";
"cf",    "il",    "or"]
assert_isequal(R, REF);
%=============================================================================
R = ["ad",    "gf",    "mp";
"be",    "hk",    "nq";
"cf",    "il",    "or"];
R{2, 2} = '';
REF = ["ad",    "gf",    "mp";
"be",    "",    "nq";
"cf",    "il",    "or"]
assert_isequal(R, REF);
%=============================================================================
R = ["ad",    "gf",    "mp";
"be",    "hk",    "nq";
"cf",    "il",    "or"];
assert_checkerror('R{2, 2} = "";', _('{} assignment expects a character vector.'));
%=============================================================================