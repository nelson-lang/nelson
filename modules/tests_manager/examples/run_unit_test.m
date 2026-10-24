%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Create and execute a small unit test in the temporary directory.
filename = [tempdir(), 'test_nelson_example.m'];
filewrite(filename, 'asserts.isequal(sum(1:4), 10);');
status = test_run(filename);
if status ~= 1
  error('The generated unit test failed.');
end
disp(['Test status: ', int2str(status)]);
