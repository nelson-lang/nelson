%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
assert_isequal(nargin('typecast'), 2);
assert_isequal(nargout('typecast'), 1);
%=============================================================================
R = typecast(single(1), 'uint32');
assert_isequal(R, uint32(1065353216));
assert_isequal(class(R), 'uint32');
%=============================================================================
R = typecast(uint32(1065353216), 'single');
assert_isequal(R, single(1));
%=============================================================================
R = typecast(double(1), 'uint8');
assert_isequal(R, uint8([0 0 0 0 0 0 240 63]));
%=============================================================================
R = typecast(uint8([0 0 0 0 0 0 240 63]), 'double');
assert_isequal(R, 1);
%=============================================================================
R = typecast(int16(-1), 'uint8');
assert_isequal(R, uint8([255 255]));
%=============================================================================
% a column vector stays a column
R = typecast(uint8([1; 0; 0; 0]), 'uint32');
assert_isequal(R, uint32(1));
assert_isequal(size(R), [1 1]);
%=============================================================================
% a row vector stays a row
R = typecast(uint16([1 2]), 'uint8');
assert_isequal(R, uint8([1 0 2 0]));
assert_isequal(size(R), [1 4]);
%=============================================================================
% a column vector with several output elements stays a column
R = typecast(uint8([1; 0; 0; 0; 2; 0; 0; 0]), 'uint32');
assert_isequal(R, uint32([1; 2]));
assert_isequal(size(R), [2 1]);
%=============================================================================
R = typecast(uint8([]), 'uint32');
assert_isequal(R, uint32([]));
%=============================================================================
assert_checkerror('typecast(uint8([1 2 3]), ''uint32'')', ...
  'The first input must contain a multiple of 4 elements to convert from real uint8 (8 bits) to real uint32 (32 bits).');
%=============================================================================
assert_checkerror('typecast(uint8([1 2;3 4]), ''uint16'')', ...
  'First argument must be a vector or empty.');
%=============================================================================
assert_checkerror('typecast(1)', _('Not enough input arguments.'));
%=============================================================================
