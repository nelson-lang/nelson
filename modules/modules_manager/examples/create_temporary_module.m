%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Create, load, test and unload a minimal module under the temporary directory.
root = [tempdir(), 'nelson-example-module/'];
if isdir(root), rmdir(root, 's'); end
mkdir(root); mkdir([root, 'etc']); mkdir([root, 'functions']); mkdir([root, 'tests']);
filewrite([root, 'module.json'], '{"title":"Temporary example module","version":"1.0.0"}');
filewrite([root, 'etc/startup.m'], 'addpath([modulepath(''example_module''), ''/functions'']);');
filewrite([root, 'etc/finish.m'], 'rmpath([modulepath(''example_module''), ''/functions'']);');
filewrite([root, 'functions/example_square.m'], 'function y = example_square(x), y = x .^ 2; end');
filewrite([root, 'tests/test_example_square.m'], ...
  'addpath([fileparts(mfilename(''fullpathext'')), ''/../functions'']); asserts.isequal(example_square(4), 16);');
addmodule(root, 'example_module');
status = test_run([root, 'tests/test_example_square.m']);
removemodule('example_module');
if status ~= 1
  error('The temporary module test failed.');
end
disp(['Temporary module test status: ', int2str(status)]);
