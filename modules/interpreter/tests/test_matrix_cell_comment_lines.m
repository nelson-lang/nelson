%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Two or more consecutive comment-only lines inside a matrix '[' or cell '{'
% literal must act as a single row break. A single comment
% line has always worked; the runs of two/three (leading, interspersed and
% trailing) used to raise a spurious "matrix/cell definition" parse error.
%=============================================================================
% single comment line (regression: must keep working)
m1 = [ %one
    1 2 3];
assert_isequal(m1, [1 2 3]);
%=============================================================================
c1 = { %one
    1, 2};
assert_isequal(numel(c1), 2);
%=============================================================================
% two leading comment lines
m2 = [ %first
    %second
    1, 2];
assert_isequal(m2, [1 2]);
%=============================================================================
c2 = { %first
    %second
    1, 2};
assert_isequal(size(c2), [1 2]);
%=============================================================================
% three leading comment lines
m3 = [ %a
    %b
    %c
    1 2 3];
assert_isequal(m3, [1 2 3]);
%=============================================================================
c3 = { %a
    %b
    %c
    'x', 'y'};
assert_isequal(size(c3), [1 2]);
%=============================================================================
% comment lines interspersed between rows
m4 = [1 2
    %comment a
    %comment b
    3 4];
assert_isequal(m4, [1 2; 3 4]);
%=============================================================================
c4 = {1, 2
    %x
    %y
    3, 4};
assert_isequal(size(c4), [2 2]);
%=============================================================================
% comment lines trailing before the closing bracket
m5 = [1 2 3
    %trailing one
    %trailing two
];
assert_isequal(m5, [1 2 3]);
%=============================================================================
% blank (empty) lines collapse the same way
m6 = [1 2

    3 4];
assert_isequal(m6, [1 2; 3 4]);
%=============================================================================
% nested literals, each with its own run of comment lines
c5 = { %outer 1
    %outer 2
    [ %inner 1
      %inner 2
      1 2], 3};
assert_isequal(numel(c5), 2);
assert_isequal(c5{1}, [1 2]);
%=============================================================================
