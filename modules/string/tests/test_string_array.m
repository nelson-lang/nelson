%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
R = strings(3, 3);
asserts.isequal(R{1}, '');
asserts.isequal(R(1), "");
asserts.checkerror('R{1} = 3', _('Curly brace assignment into a string expects a character vector.'));
asserts.checkerror('R{1} = 3', _('Curly brace assignment into a string expects a character vector.'), 'Nelson:string:UnsupportedCurlyAsgnValue');
R(1) = '3';
%=============================================================================
R = ["alpha"; "beta"];
R(1) = 'Nelson';
REF = ["Nelson"; "beta"];
asserts.isequal(R, REF);
%=============================================================================
R = ["alpha"; "beta"];
R(1) = "Nelson";
REF = ["Nelson"; "beta"];
asserts.isequal(R, REF);
%=============================================================================
R = ["Nelson"; "Engine"];
for k = 1:1000
  R(1) = "Nelson";
end
REF = ["Nelson"; "Engine"];
asserts.isequal(R, REF);
%=============================================================================
R = strings(5, 1);
R(:) = string('token');
REF = ["token"; "token"; "token"; "token"; "token"];
asserts.isequal(R, REF);
%=============================================================================
R = strings(128, 1);
R(:) = string('token');
asserts.isequal(sum(R == string('token')), 128);
asserts.isequal(R(128), string('token'));
%=============================================================================
R = strings(3, 3);
R(:, 2) = string('x');
REF = ["", "x", ""; "", "x", ""; "", "x", ""];
asserts.isequal(R, REF);
%=============================================================================
R = strings(1, 5);
R(2:4) = string('y');
REF = ["", "y", "y", "y", ""];
asserts.isequal(R, REF);
%=============================================================================
R = strings(4, 3);
R(:) = string('token');
REF = ["token", "token", "token"; "token", "token", "token"; "token", "token", "token"; "token", "token", "token"];
asserts.isequal(R, REF);
%=============================================================================
R = strings(2, 2);
R(:) = "base";
S = R;
R(:) = "copy";
asserts.isequal(size(R), [2 2]);
asserts.isequal(R, ["copy", "copy"; "copy", "copy"]);
asserts.isequal(S, ["base", "base"; "base", "base"]);
%=============================================================================
R = strings(1, 6);
R(:) = string('base');
R(2:2:6) = 'char';
REF = ["base", "char", "base", "char", "base", "char"];
asserts.isequal(R, REF);
%=============================================================================
R = strings(2, 2);
R(:) = 'char';
REF = ["char", "char"; "char", "char"];
asserts.isequal(R, REF);
%=============================================================================
R = strings(1, 5);
R(:) = "base";
R([2 4]) = "x";
REF = ["base", "x", "base", "x", "base"];
asserts.isequal(R, REF);
%=============================================================================
C = 'Nelson';
R = string(C);
C(1) = 'X';
asserts.isequal(R, "Nelson");
%=============================================================================
S = ['open'; ' is '; 'not '; 'free'];
R = string(S);
REF = ["open"; " is "; "not "; "free"];
asserts.isequal(R, REF);
%=============================================================================
A = 'abcdefghifklmnopqr';
B = reshape(A, 3, 2, 3);
R = string(B);
REF = ["ad", "gf", "mp";
  "be", "hk", "nq";
  "cf", "il", "or"];
asserts.isequal(R, REF);
%=============================================================================
R = string({'Jan', 'Feb'});
R(5) = 'Jun';
REF = ["Jan", "Feb", string(NaN), string(NaN), "Jun"];
asserts.isequal(R, REF);
%=============================================================================
R(5, 5) = 'May';
REF = ["Jan", "Feb", string(NaN), string(NaN), "Jun";
  string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
  string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
  string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
  string(NaN), string(NaN), string(NaN), string(NaN), "May"];
asserts.isequal(R, REF);
%=============================================================================
R = ["ad", "gf", "mp";
  "be", "hk", "nq";
  "cf", "il", "or"];
R(2, 2) = '';
REF = ["ad", "gf", "mp";
  "be", "", "nq";
  "cf", "il", "or"]
asserts.isequal(R, REF);
%=============================================================================
R = ["ad", "gf", "mp";
  "be", "hk", "nq";
  "cf", "il", "or"];
R(2, 2) = "";
REF = ["ad", "gf", "mp";
  "be", "", "nq";
  "cf", "il", "or"]
asserts.isequal(R, REF);
%=============================================================================
R = ["ad", "gf", "mp";
  "be", "hk", "nq";
  "cf", "il", "or"];
R{2, 2} = '';
REF = ["ad", "gf", "mp";
  "be", "", "nq";
  "cf", "il", "or"]
asserts.isequal(R, REF);
%=============================================================================
R = ["ad", "gf", "mp";
  "be", "hk", "nq";
  "cf", "il", "or"];
asserts.checkerror('R{2, 2} = "";', _('Curly brace assignment into a string expects a character vector.'));
asserts.checkerror('R{2, 2} = "";', _('Curly brace assignment into a string expects a character vector.'), 'Nelson:string:UnsupportedCurlyAsgnValue');
%=============================================================================
