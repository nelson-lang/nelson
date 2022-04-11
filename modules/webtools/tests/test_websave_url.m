%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
o = weboptions('RequestMethod', 'get');
filename = [tempdir(), 'test_websave_args.json'];
fullname = websave(filename, 'http://httpbin.org/get', 'r', i, "b+", 3, o);
R = jsondecode(fileread(fullname));
assert_istrue(isstruct(R.args));
assert_isequal(fieldnames(R.args), {'b_'; 'r'});
assert_isequal(R.args.b_, '3');
assert_isequal(R.args.r, '0+1i');
%=============================================================================
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'csv');
filename = [tempdir(), 'test_websave_args.json'];
M = [1 2 3; 4 5 6];
fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
R = jsondecode(fileread(fullname))
%=============================================================================
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'json');
filename = [tempdir(), 'test_websave_args.json'];
M = [1 2 3; 4 5 6];
fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
R = jsondecode(fileread(fullname))
%=============================================================================
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'repeating');
filename = [tempdir(), 'test_websave_args.json'];
M = [1 2 3; 4 5 6];
fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
R = jsondecode(fileread(fullname))
%=============================================================================
o = weboptions('RequestMethod', 'get', 'ArrayFormat', 'php');
filename = [tempdir(), 'test_websave_args.json'];
M = [1 2 3; 4 5 6];
fullname = websave(filename, 'http://httpbin.org/get', 'r', M, o);
R = jsondecode(fileread(fullname))
%=============================================================================
