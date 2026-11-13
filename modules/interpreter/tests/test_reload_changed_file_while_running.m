%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--FILE WATCHER REQUIRED-->
% A file changed on disk is reloaded by its next call, also inside running code.
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'File watcher service is not available in the portable runtime.');
%=============================================================================
previousPath = path();
T = tempname();
mkdir(T);
mkdir(fullfile(T, 'private'));
fun = @(v) {'function r = running_reload_fun()'; sprintf('r = %d;', v); 'end'};
cls = @(v) {'classdef running_reload_cls'; 'methods'; 'function r = w(obj)'; sprintf('r = %d;', v); 'end'; 'end'; ...
  'methods (Static)'; 'function r = v()'; sprintf('r = %d;', v); 'end'; 'end'; 'end'};
priv = @(v) {'function r = running_reload_priv()'; sprintf('r = %d;', v); 'end'};
filewrite(fullfile(T, 'running_reload_fun.m'), fun(1));
filewrite(fullfile(T, 'running_reload_script.m'), {'running_reload_value = 1;'});
filewrite(fullfile(T, 'running_reload_cls.m'), cls(1));
filewrite(fullfile(T, 'running_reload_caller.m'), ...
  {'function r = running_reload_caller()'; 'r = running_reload_priv();'; 'end'});
filewrite(fullfile(T, 'private', 'running_reload_priv.m'), priv(1));
addpath(T);
%=============================================================================
running_reload_script;
obj = running_reload_cls();
asserts.isequal([running_reload_fun(), running_reload_value, running_reload_cls.v(), obj.w(), ...
  running_reload_caller()], [1 1 1 1 1]);
%=============================================================================
% written by Nelson: the next call sees the new file
filewrite(fullfile(T, 'running_reload_fun.m'), fun(33));
filewrite(fullfile(T, 'running_reload_script.m'), {'running_reload_value = 33;'});
filewrite(fullfile(T, 'running_reload_cls.m'), cls(33));
filewrite(fullfile(T, 'private', 'running_reload_priv.m'), priv(33));
running_reload_script;
asserts.isequal([running_reload_fun(), running_reload_value, running_reload_cls.v(), obj.w(), ...
  running_reload_caller()], [33 33 33 33 33]);
%=============================================================================
% inside a loop, from the call after the write
r = zeros(1, 4);
for k = 1:4
  if k == 3
    fid = fopen(fullfile(T, 'running_reload_fun.m'), 'wt');
    fprintf(fid, 'function r = running_reload_fun()\nr = 44;\nend\n');
    fclose(fid);
  end
  r(k) = running_reload_fun();
end
asserts.isequal(r, [33 33 44 44]);
copyfile(fullfile(T, 'running_reload_fun.m'), fullfile(T, 'running_reload_saved.txt'));
filewrite(fullfile(T, 'running_reload_fun.m'), fun(55));
asserts.isequal(running_reload_fun(), 55);
copyfile(fullfile(T, 'running_reload_saved.txt'), fullfile(T, 'running_reload_fun.m'), 'f');
asserts.isequal(running_reload_fun(), 44);
%=============================================================================
% written by another process: picked up while the code runs
src = fullfile(T, 'running_reload_new.txt');
filewrite(src, fun(66));
if ispc()
  [status, ~] = system(['copy /Y "', src, '" "', fullfile(T, 'running_reload_fun.m'), '"']);
else
  [status, ~] = system(['cp -f "', src, '" "', fullfile(T, 'running_reload_fun.m'), '"']);
end
asserts.isequal(status, 0);
t0 = tic();
value = running_reload_fun();
while value ~= 66 && toc(t0) < 10
  value = running_reload_fun();
end
asserts.isequal(value, 66);
%=============================================================================
path(previousPath);
clear('obj', 'running_reload_fun', 'running_reload_caller', 'running_reload_cls');
rmdir(T, 's');
%=============================================================================
